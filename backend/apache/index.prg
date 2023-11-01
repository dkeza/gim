#include "hbclass.ch"

FUNCTION Main()

     LOCAL jsonOut := {=>}
     LOCAL jsonIn := {=>}
     LOCAL input := ""
     LOCAL jsonFound := 0
     LOCAL oSession := cSession():New()
     LOCAL oGym := cGym():New(oSession)
     LOCAL oUser := cUser():New(oSession, oGym)
     LOCAL oEquipment := cEquipment():New(oSession)

     jsonOut["success"] := .F.
     jsonOut["error"] := ""
     jsonOut["errorcode"] := 0

     AP_SetContentType("application/json")
    
     IF AP_Method()<>"POST"
          jsonOut["error"] := "http_method_not_allowed"
          jsonOut["errorcode"] := 1
          ?? hb_jsonEncode(jsonOut)
          RETURN nil
     ENDIF
    
     input = AP_Body()
     
     IF LEN(input)>0
          jsonFound := hb_jsonDecode(input, @jsonIn)
          // hb_MemoWrit(PathBase() + "\test.log", TRANSFORM(jsonFound, "@"))
     ENDIF

     DO CASE

          CASE LEN(input)==0 .OR. jsonFound==0
               jsonOut["error"] := "unknown_request"
               jsonOut["errorcode"] := 2

          CASE jsonIn["method"]=="ping"
               jsonOut["timestamp"] := ""
               OnPing(@jsonIn, @jsonOut)

          CASE jsonIn["method"]=="register"
               oUser:OnRegister(@jsonIn, @jsonOut)

          CASE jsonIn["method"]=="login"
               oUser:OnLogin(@jsonIn, @jsonOut)

          CASE .NOT. oSession:IsValid()
               jsonOut["error"] := "unauthenticated"
               jsonOut["errorcode"] := 3

          CASE jsonIn["method"]=="logout"
               oUser:OnLogout(@jsonOut)

          CASE jsonIn["method"]=="gym_list"
               oGym:List(@jsonOut)

          CASE jsonIn["method"]=="gym_create"
               oGym:Create(@jsonIn, @jsonOut)

          CASE jsonIn["method"]=="user_select_gym"
               oUser:OnSelectGym(@jsonIn, @jsonOut)

          CASE jsonIn["method"]=="equipment_list"
               oEquipment:List(@jsonOut)

          CASE jsonIn["method"]=="equipment_create"
               oEquipment:Create(@jsonIn, @jsonOut)

               OTHERWISE

               jsonOut["error"] := "unknown_method"
               jsonOut["errorcode"] := 4

     ENDCASE
	
     ?? hb_jsonEncode(jsonOut) 

RETURN nil

//

FUNCTION OnPing(jsonIn, jsonOut)

     jsonOut["success"] := .T.
     jsonOut["timestamp"] := DTOS(DATE()) + "_" + TIME()

RETURN nil

//

FUNCTION tbopen(cTable)

     SELECT 0
     USE (hb_GetEnv("PRGPATH") + "/" + cTable) SHARED

RETURN .T.

FUNCTION tbclose(cTable)

     CLOSE &cTable

RETURN .T.

// --- CLASS cUser --- BEGIN

CLASS cUser

     DATA oSession INIT nil
     DATA oGym INIT nil
	
     METHOD New(oSession) CONSTRUCTOR	
     METHOD OnRegister(jsonIn, jsonOut)
     METHOD OnLogin(jsonIn, jsonOut)
     METHOD OnLogout(jsonOut)
     METHOD ValidPassword(jsonIn, jsonOut, iduser)
     METHOD GenerateSalt(nLength)
     METHOD HashPassword(cPassword, cSalt)
     METHOD OnSelectGym(jsonIn, jsonOut)

ENDCLASS

METHOD New(oSession, oGym) CLASS cUser

     ::oSession = oSession
     ::oGym = oGym

RETURN Self

METHOD OnRegister(jsonIn, jsonOut) CLASS cUser

     LOCAL cSalt, cHashedPassword, cEMail, cUsername, cPassword

     cUsername := AllTrim(jsonIn["nick"])
     cPassword := AllTrim(jsonIn["password"])
     cEMail := AllTrim(jsonIn["email"])

     IF LEN(cUsername) == 0
          jsonOut["error"] := "invalid_username"
          jsonOut["errorcode"] := 8
          RETURN nil
     ENDIF

     IF LEN(cPassword) == 0
          jsonOut["error"] := "invalid_password"
          jsonOut["errorcode"] := 9
          RETURN nil
     ENDIF

     IF LEN(cEMail) == 0
          jsonOut["error"] := "invalid_email"
          jsonOut["errorcode"] := 10
          RETURN nil
     ENDIF

     // Generate a new salt
     cSalt = ::GenerateSalt(16)
     
     // Hash the provided password with the new salt
     cHashedPassword = ::HashPassword(cPassword, cSalt)
     
     // Store in the database
     tbopen("users")
     SELECT users

     LOCATE FOR users->nick == PADR(cUsername,20)
     IF FOUND()
          jsonOut["error"] := "username_exists"
          jsonOut["errorcode"] := 11
          return nil
     ENDIF

     IF .NOT. RLOCK()
          jsonOut["error"] := "database_access_error"
          jsonOut["errorcode"] := 12
          return nil
     ENDIF

     APPEND BLANK

     users->nick := cUsername
     users->email := cEMail
     users->firstname := jsonIn["firstname"]
     users->lastname := jsonIn["lastname"]
     users->created := hb_DateTime()
     users->salt := cSalt
     users->hash := cHashedPassword

     jsonOut["nick"] := users->nick
     jsonOut["firstname"] := users->firstname
     jsonOut["lastname"] := users->lastname
     jsonOut["email"] := users->email
     jsonOut["success"] := .T.

     DbUnLock()
       
     ::oSession:Create(users->id, @jsonOut)

RETURN nil

//

METHOD OnLogin(jsonIn, jsonOut) CLASS cUser

     LOCAL iduser, aGym, gymID, gymName

     iduser := 0

     jsonOut["nick"] := ""
     jsonOut["firstname"] := ""
     jsonOut["lastname"] := ""
     jsonOut["email"] := ""

     IF .NOT. ::ValidPassword(@jsonIn, @jsonOut, @iduser, @gymID, @gymName)
          RETURN nil
     ENDIF

     ::oSession:Create(iduser, @jsonOut)

     aGym = {=>}
     aGym['id'] = gymid
     aGym['name'] = gymName
     aGym['list'] = ::oGym:GetList()

     jsonOut["gym"] := aGym

RETURN nil

//

METHOD OnLogout(jsonOut) CLASS cUser

     jsonOut["success"] := .T.

     ::oSession:Delete()

RETURN nil

//

METHOD OnSelectGym(jsonIn, jsonOut) CLASS cUser

     LOCAL aGym

     IF EMPTY(jsonIn["id"])
          jsonOut["error"] := "invalid_gym_id"
          jsonOut["errorcode"] := 17
          RETURN nil
     ENDIF

     aGym := ::oGym:Get(jsonIn["id"], ::oSession:nUserID)

     IF EMPTY(aGym["id"])
          jsonOut["error"] := "invalid_gym_id"
          jsonOut["errorcode"] := 18
          RETURN nil
     ENDIF

     tbopen("users")

     LOCATE FOR users->id == ::oSession:nUserID
     IF ! FOUND()
          tbclose("users")
          jsonOut["error"] := "invalid_user"
          jsonOut["errorcode"] := 15
          RETURN nil
     ENDIF

     IF .NOT. RLOCK()
          jsonOut["error"] := "database_access_error"
          jsonOut["errorcode"] := 16
     ENDIF

     field->gymid := aGym["id"]

     DbUnLock()

     tbclose("users")

     jsonOut["success"] := .T.
     jsonOut["id"] := aGym["id"]
     jsonOut["name"] := aGym["name"]

RETURN nil

METHOD ValidPassword(jsonIn, jsonOut, iduser, gymID, gymName) CLASS cUser

     LOCAL cStoredSalt, cStoredHash, cHashedAttempt, cUsername, cPassword, aGym

     cUsername := AllTrim(jsonIn["nick"])
     cPassword := AllTrim(jsonIn["password"])
     gymID := ""
     gymName := ""

     IF LEN(cUsername) == 0 .OR. LEN(cPassword) == "0"
          jsonOut["error"] := "empty_username_or_password"
          jsonOut["errorcode"] := 5
          RETURN nil
     ENDIF

     // Retrieve the stored salt and hash from the database
     tbopen("users")
     LOCATE FOR users->nick = cUsername
     IF !FOUND()
          tbclose("users")
          jsonOut["error"] := "invalid_username"
          jsonOut["errorcode"] := 6
          RETURN .F.  // User not found
     ENDIF
  
     cStoredSalt = ALLTRIM(users->salt)
     cStoredHash = ALLTRIM(users->hash)
  
     // Hash the provided password with the retrieved salt
     cHashedAttempt = ::HashPassword(cPassword, cStoredSalt)
  
     // Check if this hash matches the stored hash
     IF .NOT. cHashedAttempt == cStoredHash
          jsonOut["error"] := "invalid_password"
          jsonOut["errorcode"] := 7
          RETURN .F.
     ENDIF

     iduser := users->id
     jsonOut["nick"] := ALLTRIM(users->nick)
     jsonOut["firstname"] := ALLTRIM(users->firstname)
     jsonOut["lastname"] := ALLTRIM(users->lastname)
     jsonOut["email"] := ALLTRIM(users->email)
     jsonOut["success"] := .T.

     gymID := users->gymid

     IF gymID > 0
          aGym := ::oGym:Get(gymID)
          gymName := aGym["name"]
     ENDIF

     tbclose("users")

RETURN .T.

METHOD GenerateSalt(nLength) CLASS cUser

     LOCAL cSalt := ""
     LOCAL nI
     LOCAL cChars := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
     
     FOR nI := 1 TO nLength
          cSalt += SUBSTR(cChars, RAND() * LEN(cChars) + 1, 1)
     NEXT
     
RETURN cSalt

METHOD HashPassword(cPassword, cSalt) CLASS cUser
RETURN hb_SHA1(cPassword + cSalt, "SHA256")

// --- CLASS cUser    --- END

// --- CLASS cSession --- BEGIN

CLASS cSession

     DATA cID  INIT ""
     DATA nUserID INIT 0
	
     METHOD New() CONSTRUCTOR	
     METHOD ReadCookie()
     METHOD Create(duser, jsonOut)
     METHOD Delete()
     METHOD IsValid()
     METHOD GenerateUUID() 

ENDCLASS

METHOD New() CLASS cSession

     ::ReadCookie()

RETURN Self

METHOD ReadCookie() CLASS cSession

     LOCAL hHeadersIn, cIDValue

     ::cID := ""
     hHeadersIn := AP_HeadersIn()

     IF .NOT. hb_HHasKey(hHeadersIn, "Cookie")
          RETURN nil
     ENDIF

     cIDValue := hHeadersIn["Cookie"]

     IF LEN(cIDValue)<30
          RETURN nil
     ENDIF

     ::cID := RIGHT(cIDValue, 27)

RETURN nil

METHOD Create(iduser, jsonOut) CLASS cSession

     LOCAL cCookie
     LOCAL sid := ""

     cCookie := ""
     sid := ::GenerateUUID()

     SetCookie('id', sid)
          
     tbopen("sessions")

     APPEND BLANK
     IF .NOT. RLOCK()
          jsonOut["error"] := "database_access_error"
          jsonOut["errorcode"] := 13
     ENDIF

     ::cID := sid
     ::nUserID = iduser

     field->sessionid := sid
     field->userid := iduser
     field->created := hb_DateTime()

     DbUnLock()

     tbclose("sessions")
     // cCookie := GetCookieByKey( "_APP_SESSION_" )
     // SetCookie( "_APP_SESSION_", cCookie )

RETURN nil

METHOD IsValid() CLASS cSession

     LOCAL isValid := .T.

     IF LEN(::cID)==0
          isValid := .F.
     ENDIF

     tbopen("sessions")

     LOCATE FOR sessions->sessionid = ::cID
     IF FOUND()
          isValid := .T.
          ::nUserID = sessions->userid
     ENDIF

     tbclose("sessions")

RETURN isValid

METHOD GenerateUUID() CLASS cSession

     LOCAL cChars := "0123456789ABCDEF"
     LOCAL cUUID  := ""
     LOCAL n := 0
  
     FOR n = 1 to 8
          cUUID += SubStr( cChars, hb_Random( 1, 16 ), 1 )
     NEXT
     
     FOR n = 1 to 3
          cUUID += SubStr( cChars, hb_Random( 1, 16 ), 1 )
     NEXT
  
     FOR n = 1 to 4
          cUUID += SubStr( cChars, hb_Random( 1, 16 ), 1 )
     NEXT
  
     FOR n = 1 to 12
          cUUID += SubStr( cChars, hb_Random( 1, 16 ), 1 )
     NEXT
   
RETURN LOWER(cUUID)

METHOD Delete() CLASS cSession

     IF LEN(::cID)==0
          RETURN nil
     ENDIF

     tbopen("sessions")
     LOCATE FOR sessions->sessionid = ::cID
     IF FOUND() .AND. RLOCK()
          DELETE
     ENDIF

     SetCookie('id', "") 

     tbclose("sessions")

RETURN Self

// --- CLASS cSession --- END

// --- CLASS cGym   --- BEGIN

CLASS cGym

     DATA oSession INIT nil
	
     METHOD New(oSession) CONSTRUCTOR	
     METHOD List(jsonOut)
     METHOD Get(gymID, userID)
     METHOD GetList()
     METHOD Create(jsonIn, jsonOut)

ENDCLASS

METHOD New(oSession) CLASS cGym

     ::oSession = oSession

RETURN Self

METHOD List(jsonOut) CLASS cGym

     jsonOut["success"] := .T.
     jsonOut["list"] := ::GetList()

RETURN nil

METHOD Get(gymID, userID) CLASS cGym
     LOCAL aResult := {}
     LOCAL cID, cName
     LOCAL aData

     aData = {=>}
     aData['id'] = ""
     aData['name'] = ""

     IF EMPTY(gymID)
          RETURN aData
     ENDIF

     tbopen("gyms")

     LOCATE FOR gyms->id == gymID .AND. (EMPTY(userID) .OR. gyms->userid == userID)
     IF FOUND()
          aData['id'] = gyms->id
          aData['name'] = ALLTRIM(gyms->name)
     ENDIF

     tbclose("gyms")

RETURN aData

METHOD GetList() CLASS cGym
     LOCAL aResult := {}
     LOCAL cID, cName
     LOCAL aReg
     
     tbopen("gyms")
     dbGoTop()

     DO WHILE ! EOF()
          IF gyms->userid == ::oSession:nUserID
               aReg = {=>}
               aReg[ 'id' ] = gyms->id
               aReg[ 'name'  ] = AllTrim(gyms->name)
               AAdd( aResult, aReg )
          ENDIF
          dbSkip()
     ENDDO
     
     tbclose("gyms")

RETURN aResult

METHOD Create(jsonIn, jsonOut) CLASS cGym
          
     tbopen("gyms")

     APPEND BLANK
     IF .NOT. RLOCK()
          jsonOut["error"] := "database_access_error"
          jsonOut["errorcode"] := 14
     ENDIF

     field->name := jsonIn["name"]
     field->userid := ::oSession:nUserID
     field->created := hb_DateTime()

     DbUnLock()

     tbclose("gyms")

     jsonOut["success"] := .T.

RETURN nil

// --- CLASS cGym   ---   END

// --- CLASS cEquipment   --- BEGIN

CLASS cEquipment

     DATA oSession INIT nil
	
     METHOD New(oSession) CONSTRUCTOR	
     METHOD List(jsonOut)
     METHOD Get(equipmentID, userID)
     METHOD GetList()
     METHOD Create(jsonIn, jsonOut)

ENDCLASS

METHOD New(oSession) CLASS cEquipment

     ::oSession = oSession

RETURN Self

METHOD List(jsonOut) CLASS cEquipment

     jsonOut["success"] := .T.
     jsonOut["list"] := ::GetList()

RETURN nil

METHOD Get(equipmentID, userID) CLASS cEquipment
     LOCAL aResult := {}
     LOCAL cID, cName
     LOCAL aData

     aData = {=>}
     aData['id'] = ""
     aData['name'] = ""

     IF EMPTY(equipmentID)
          RETURN aData
     ENDIF

     tbopen("equipments")

     LOCATE FOR equipments->id == equipmentID .AND. (EMPTY(userID) .OR. equipments->userid == userID)
     IF FOUND()
          aData['id'] = equipments->id
          aData['name'] = ALLTRIM(equipments->name)
     ENDIF

     tbclose("equipments")

RETURN aData

METHOD GetList() CLASS cEquipment
     LOCAL aResult := {}
     LOCAL cID, cName
     LOCAL aReg
     
     tbopen("equipments")
     dbGoTop()

     DO WHILE ! EOF()
          IF equipments->userid == ::oSession:nUserID
               aReg = {=>}
               aReg[ 'id' ] = equipments->id
               aReg[ 'name'  ] = AllTrim(equipments->name)
               AAdd( aResult, aReg )
          ENDIF
          dbSkip()
     ENDDO
     
     tbclose("equipments")

RETURN aResult

METHOD Create(jsonIn, jsonOut) CLASS cEquipment
          
     tbopen("equipments")

     APPEND BLANK
     IF .NOT. RLOCK()
          jsonOut["error"] := "database_access_error"
          jsonOut["errorcode"] := 200
     ENDIF

     field->name := jsonIn["name"]
     field->userid := ::oSession:nUserID
     field->created := hb_DateTime()

     DbUnLock()

     tbclose("equipments")

     jsonOut["success"] := .T.

RETURN nil

// --- CLASS cEquipment   ---   END