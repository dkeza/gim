FUNCTION Main()
	LOCAL jsonOut := {=>}
     LOCAL jsonIn := {=>}
     LOCAL input := ""
     LOCAL hHeadersIn := ""
     LOCAL jsonFound := 0
     PRIVATE idSession := ""

	jsonOut["success"] := .F.
     jsonOut["error"]   := ""

     AP_SetContentType( "application/json" )
    
     IF AP_Method()<>"POST"
          jsonOut["error"] := "HTTP method not allowed"
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
               jsonOut["error"] := "Unknown request"
               ?? hb_jsonEncode( jsonOut )
               RETURN nil
          CASE jsonIn["method"]=="login"
               jsonOut["user"] := ""
               jsonOut["name"] := ""
               OnLogin(@jsonIn, @jsonOut)
          CASE .NOT. ValidSessionID()
               jsonOut["error"] := "Unauthenticated"
               ?? hb_jsonEncode( jsonOut )
               RETURN nil
          CASE jsonIn["method"]=="ping"
               jsonOut["timestamp"] := ""
               OnPing(@jsonIn, @jsonOut)
          OTHERWISE
               jsonOut["error"] := "Unknown method"
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

     USE ( hb_GetEnv( "PRGPATH" ) + "/sessions" ) SHARED
     LOCATE FOR sessions->sessionid = idSession
     IF .NOT. FOUND()
          isValid := .F.
     ENDIF
RETURN isValid

FUNCTION OnLogin(jsonIn, jsonOut)
     LOCAL cCookie := ""
     LOCAL id := ""

     IF jsonIn["user"] == "admin" .AND. jsonIn["password"] == "123"
          jsonOut["success"] := .T.
          jsonOut["user"] 	:= jsonIn["user"]
          jsonOut["name"] 	:= "Administrator"

          id := GenerateUUID()
          SetCookie('id', id)
          
          USE ( hb_GetEnv( "PRGPATH" ) + "/sessions" ) SHARED

          APPEND BLANK

          IF RLOCK()
              field->sessionid    := id
              field->userid    := jsonIn["user"]
              field->created := hb_DateTime()
              DbUnLock()
          ENDIF

          * cCookie := GetCookieByKey( "_APP_SESSION_" )
          * SetCookie( "_APP_SESSION_", cCookie )

     ELSE
          jsonOut["error"] := "Login failed"
     ENDIF

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

FUNCTION OnPing(jsonIn, jsonOut)
     jsonOut["success"] := .T.
     jsonOut["timestamp"] := DTOS(DATE()) + "_" + TIME()
RETURN nil