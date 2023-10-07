FUNCTION Main()
	LOCAL jsonOut := {=>}
     LOCAL jsonIn := {=>}
     LOCAL input := ""
     LOCAL hHeadersIn := ""
     LOCAL jsonFound := 0
     PRIVATE idSession := ""

	jsonOut["success"] := .F.
     jsonOut["error"]   := ""
     jsonOut["errorcode"] := 0

     AP_SetContentType( "application/json" )
    
     IF AP_Method()<>"POST"
          jsonOut["error"] := "http_method_not_allowed"
          jsonOut["errorcode"] := 1
          ?? hb_jsonEncode( jsonOut )
          RETURN nil
     ENDIF

     hHeadersIn := AP_HeadersIn()
     IF hb_HHasKey( hHeadersIn, "Cookie" )
          idSession := hHeadersIn[ "Cookie" ]
          IF LEN(idSession)<30
               idSession := ""
          ELSE
               idSession := RIGHT(idSession, 27)
          ENDIF
     ENDIF
     
     input = AP_Body()
     
     IF LEN(input)>0
          jsonFound := hb_jsonDecode(input, @jsonIn)
          hb_MemoWrit(PathBase() + "\test.log", TRANSFORM(jsonFound, "@"))
     ENDIF

     DO CASE
          CASE LEN(input)==0 .OR. jsonFound==0
               jsonOut["error"] := "unknown_request"
               jsonOut["errorcode"] := 2
               ?? hb_jsonEncode( jsonOut )
               RETURN nil
          CASE jsonIn["method"]=="register"
               UserRegister(@jsonIn, @jsonOut)
          CASE jsonIn["method"]=="login"
               OnLogin(@jsonIn, @jsonOut)
          CASE .NOT. ValidSessionID()
               jsonOut["error"] := "unauthenticated"
               jsonOut["errorcode"] := 3
               ?? hb_jsonEncode( jsonOut )
               RETURN nil
          CASE jsonIn["method"]=="ping"
               jsonOut["timestamp"] := ""
               OnPing(@jsonIn, @jsonOut)
          CASE jsonIn["method"]=="logout"
               OnLogout(@jsonOut)
          OTHERWISE
               jsonOut["error"] := "unknown_method"
               jsonOut["errorcode"] := 4
               ?? hb_jsonEncode( jsonOut )
               RETURN nil
     ENDCASE
	
	?? hb_jsonEncode( jsonOut ) 

RETURN nil

FUNCTION ValidSessionID
     LOCAL isValid := .T.

     IF LEN(idSession)==0
          isValid := .F.
     ENDIF
     tbopen("sessions")
     LOCATE FOR sessions->sessionid = idSession
     IF .NOT. FOUND()
          isValid := .F.
     ENDIF
RETURN isValid

FUNCTION OnLogin(jsonIn, jsonOut)
     LOCAL iduser
     iduser := 0

     jsonOut["nick"] := ""
     jsonOut["firstname"] := ""
     jsonOut["lastname"] := ""
     jsonOut["email"] := ""

     IF .NOT. ValidPassword(@jsonIn, @jsonOut, @iduser)
          RETURN nil
     ENDIF

     SessionCreate(iduser)
RETURN nil

FUNCTION ValidPassword( jsonIn, jsonOut, iduser )
     LOCAL cStoredSalt, cStoredHash, cHashedAttempt, cUsername, cPassword

     cUsername := AllTrim(jsonIn["user"])
     cPassword := AllTrim(jsonIn["password"])

     IF LEN(cUsername) == 0 .OR. LEN(cPassword) == "0"
          jsonOut["error"] := "empty_username_or_password"
          jsonOut["errorcode"] := 5
          RETURN nil
     ENDIF

     // Retrieve the stored salt and hash from the database
     tbopen("users")
     LOCATE FOR users->nick = cUsername
     IF !FOUND()
        CLOSE users
        jsonOut["error"] := "invalid_username"
        jsonOut["errorcode"] := 6
        RETURN .F.  // User not found
     ENDIF
  
     cStoredSalt = AllTrim(salt)
     cStoredHash = AllTrim(hash)
  
     // Hash the provided password with the retrieved salt
     cHashedAttempt = HashPassword( cPassword, cStoredSalt )
  
     // Check if this hash matches the stored hash
     IF .NOT. cHashedAttempt == cStoredHash
          jsonOut["error"] := "invalid_password"
          jsonOut["errorcode"] := 7
          RETURN .F.
     ENDIF

     iduser := id
     jsonOut["nick"] := AllTrim(nick)
     jsonOut["firstname"] := AllTrim(firstname)
     jsonOut["lastname"] := AllTrim(lastname)
     jsonOut["email"] := AllTrim(email)
     jsonOut["success"] := .T.

     CLOSE users

RETURN .T.

FUNCTION UserRegister( jsonIn, jsonOut )
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
     cSalt = GenerateSalt( 16 )
     
     // Hash the provided password with the new salt
     cHashedPassword = HashPassword( cPassword, cSalt )
     
     // Store in the database
     tbopen("users")
     
     LOCATE FOR users->nick == cUsername
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

     field->nick := cUsername
     field->email := cEMail
     field->firstname := jsonIn["firstname"]
     field->lastname := jsonIn["lastname"]
     field->created := hb_DateTime()
     field->salt := cSalt
     field->hash := cHashedPassword

     DbUnLock()
       
     iduser := id
     jsonOut["nick"] := nick
     jsonOut["firstname"] := firstname
     jsonOut["lastname"] := lastname
     jsonOut["email"] := email
     jsonOut["success"] := .T.

     CLOSE users

     SessionCreate(iduser)

RETURN nil

FUNCTION SessionCreate(iduser)
     LOCAL cCookie, sid
     cCookie := ""
     sid := GenerateUUID()

     SetCookie('id', sid)
          
     tbopen("sessions")
     APPEND BLANK
     IF .NOT. RLOCK()
          jsonOut["error"] := "database_access_error"
          jsonOut["errorcode"] := 13
     ENDIF

     field->sessionid := sid
     field->userid := iduser
     field->created := hb_DateTime()

     DbUnLock()

     * cCookie := GetCookieByKey( "_APP_SESSION_" )
     * SetCookie( "_APP_SESSION_", cCookie )
RETURN nil

FUNCTION OnLogout(jsonOut)
     jsonOut["success"] := .T.

     IF LEN(idSession)==0
          RETURN nil
     ENDIF

     tbopen("sessions")
     LOCATE FOR sessions->sessionid = idSession
     IF FOUND() .AND. RLOCK()
          DELETE
     ENDIF

     SetCookie('id', "")
RETURN nil

FUNCTION GenerateUUID

   local cChars := "0123456789ABCDEF"
   local cUUID  := ""

   for n = 1 to 8
      cUUID += SubStr( cChars, hb_Random( 1, 16 ), 1 )
   next
   
   for n = 1 to 3
      cUUID += SubStr( cChars, hb_Random( 1, 16 ), 1 )
   next

   for n = 1 to 4
      cUUID += SubStr( cChars, hb_Random( 1, 16 ), 1 )
   next

   for n = 1 to 12
      cUUID += SubStr( cChars, hb_Random( 1, 16 ), 1 )
   next
 
RETURN LOWER(cUUID)

FUNCTION GenerateSalt( nLength )
     LOCAL cSalt := ""
     LOCAL nI
     LOCAL cChars := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
     
     FOR nI := 1 TO nLength
          cSalt += SUBSTR( cChars, RAND() * LEN( cChars ) + 1, 1 )
     NEXT
     
     RETURN cSalt

FUNCTION HashPassword( cPassword, cSalt )
     RETURN hb_SHA1( cPassword + cSalt, "SHA256" )

FUNCTION OnPing(jsonIn, jsonOut)
     jsonOut["success"] := .T.
     jsonOut["timestamp"] := DTOS(DATE()) + "_" + TIME()
RETURN nil

FUNCTION tbopen(cTable)
     USE ( hb_GetEnv( "PRGPATH" ) + "/" + cTable) SHARED
RETURN .T.