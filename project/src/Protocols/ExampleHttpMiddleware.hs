module Protocols.ExampleHttpMiddleware where
import Protocols.Http

testMiddelware = HttpMiddleware serverResponse

serverResponse str | str=="GET" = "GET detected"
serverResponse str | otherwise = "Found other: " ++ str