# gim

curl -i -v -H "Content-Type: application/json" -X POST http://gim.local/api/v1/index.prg -d {\"method\":\"login\",\"user\":\"admin\",\"password\":\"123\"}

curl -i -v -H "Content-Type: application/json" -H "Cookie: id=818beb95da2260e0e4c111657da" -X POST http://gim.local/api/v1/index.prg -d {\"method\":\"ping\"}