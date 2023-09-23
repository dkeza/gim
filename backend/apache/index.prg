FUNCTION Main()
	LOCAL jsonOut := {=>}
     LOCAL jsonIn := {=>}
     LOCAL input := ""
     LOCAL hHeadersIn := ""
     PRIVATE idSession := ""

	jsonOut["success"] := .F.
     jsonOut["error"]   := ""     
	jsonOut["user"]    := ""
	jsonOut["name"]    := ""

     AP_SetContentType( "application/json" )
    
     IF AP_Method()<>"POST"
          jsonOut["error"] := "HTTP method not allowed"     
          ?? hb_jsonEncode( jsonOut )
          RETURN nil
     ENDIF

     hHeadersIn := AP_HeadersIn()
     IF hb_HHasKey( hHeadersIn, "Cookie" )
          idSession := hHeadersIn[ "Cookie" ]
     ENDIF
     
     input = AP_Body()
     
     hb_jsonDecode( input, @jsonIn )

     hb_MemoWrit("c:\laragon\www\gim\api\v1\test.log", idSession + CHR(13))

     DO CASE
          CASE jsonIn["method"]=="login"
               OnLogin(@jsonIn, @jsonOut)
          OTHERWISE
               jsonOut["error"] := "Unknown method"     
               ?? hb_jsonEncode( jsonOut )
               RETURN nil
     ENDCASE
	
	?? hb_jsonEncode( jsonOut ) 

RETURN nil

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

function GenerateUUID

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
 
return LOWER(cUUID)
