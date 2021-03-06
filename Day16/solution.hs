import Data.List (foldl', intercalate)
import qualified Data.Map as Map
import System.IO

-- This times we have a lot of short samples so is more convenient to have them stored direclty here
-- TODO: do testing elsewhere

inputSample1 = "D2FE28"

binStringSample1 = "110100101111111000101000"

inputSample2 = "38006F45291200"

binStringSample2 = "00111000000000000110111101000101001010010001001000000000"

inputSample3 = "EE00D40C823060"

binStringSample3 = "11101110000000001101010000001100100000100011000001100000"

inputSample4 = "8A004A801A8002F478"

inputSample5 = "620080001611562C8802118E34"

inputSample6 = "C0015000016115A2E0802F182340"

inputSample7 = "A0016C880162017C3686B18A3D4780"

-- Type aliases and data

type Bit = Bool

type Bitstring = [Bit]

data Header = Header
  { version :: Bitstring, -- TODO: refactor this data type and use directly Int for version and Exp for typeId
    typeID :: Bitstring
  }
  deriving (Show)

data Packet = Packet
  { header :: Header,
    payload :: Payload
  }
  deriving (Show)

data Payload = Literal Bitstring | Packets [Packet] deriving (Show)

-- typeID   0      1     2     3     4     5    6    7
data Operator = Sum | Prod | Min | Max | Val | Gt | Lt | Eq deriving (Eq, Show)

-- Helper functions

getOperator :: Bitstring -> Operator
getOperator typeId
  | typeId == [False, False, False] = Sum
  | typeId == [False, False, True] = Prod
  | typeId == [False, True, False] = Min
  | typeId == [False, True, True] = Max
  | typeId == [True, False, False] = Val
  | typeId == [True, False, True] = Gt
  | typeId == [True, True, False] = Lt
  | typeId == [True, True, True] = Eq
  | otherwise = error $ "Error! getOperator - typeID not valid: " ++ showBitstring typeId

emptyBitstring :: Bitstring -> Bool
emptyBitstring = null

hexCharToBitstring :: Char -> Bitstring
hexCharToBitstring c
  | c == '0' = [False, False, False, False]
  | c == '1' = [False, False, False, True]
  | c == '2' = [False, False, True, False]
  | c == '3' = [False, False, True, True]
  | c == '4' = [False, True, False, False]
  | c == '5' = [False, True, False, True]
  | c == '6' = [False, True, True, False]
  | c == '7' = [False, True, True, True]
  | c == '8' = [True, False, False, False]
  | c == '9' = [True, False, False, True]
  | c == 'A' = [True, False, True, False]
  | c == 'B' = [True, False, True, True]
  | c == 'C' = [True, True, False, False]
  | c == 'D' = [True, True, False, True]
  | c == 'E' = [True, True, True, False]
  | c == 'F' = [True, True, True, True]
  | otherwise = error $ "Error! hexCharToBitstring - " ++ show c ++ " is not a valid Hex char"

hexStringToBitstring :: String -> Bitstring
hexStringToBitstring = concatMap hexCharToBitstring

bitstringToInt :: Bitstring -> Int
bitstringToInt = foldl' (\acc x -> acc * 2 + (if x then 1 else 0)) 0

showBitstring :: Bitstring -> String
showBitstring = map (\c -> if c then '1' else '0')

showHeader :: Header -> String
showHeader h =
  let (v, t) = (bitstringToInt $ version h, bitstringToInt $ typeID h)
   in "{version: " ++ show v ++ ", typeID: " ++ show t ++ "}"

showPayload :: Payload -> String
showPayload p = case p of
  Literal bstr -> "{Literal: " ++ show (bitstringToInt bstr) ++ "}"
  Packets pkts ->
    let m = map showPacket pkts
        s = intercalate ", " m
     in "{Packets: " ++ s ++ "}"

showPacket :: Packet -> String
showPacket p = "{header: " ++ showHeader (header p) ++ ", payload: " ++ showPayload (payload p)

-- It depends only by typeID
isLiteralPayload :: Bitstring -> Bool
isLiteralPayload typeID = [True, False, False] == typeID

-- It removes all the control bits leaving just the value and the total length of the payload
-- the length is needed to decode operator packets
parseLiteralPayload :: Bitstring -> (Int, Bitstring)
parseLiteralPayload bstr =
  let decode (controlBit : b0 : b1 : b2 : b3 : rest) decoded len =
        let decoded' = decoded ++ [b0, b1, b2, b3]
            len' = len + 5
         in if controlBit
              then decode rest decoded' len'
              else (len', decoded')
      decode _ _ _ = error "Error! parseLiteralPayload - Corrupted input bitstring"
   in decode bstr [] 0

parsePacketsPayloadWithNumber :: Int -> Bitstring -> ([Packet], Bitstring, Int)
parsePacketsPayloadWithNumber num rawPackets =
  let extract 0 rest pks totalLen = (pks, rest, totalLen)
      extract n rawPks pks totalLen =
        let (pk, rest, len) = parsePacket rawPks
         in extract (n -1) rest (pks ++ [pk]) (totalLen + len)
   in extract num rawPackets [] 0

parsePacketsPayloadWithLength :: Int -> Bitstring -> ([Packet], Bitstring, Int)
parsePacketsPayloadWithLength payloadLen rawPackets =
  let extract 0 rest pks totalLen = (pks, rest, totalLen)
      extract l rawPks pks totalLen =
        let (pk, rest, parsedLen) = parsePacket rawPks
         in extract (l - parsedLen) rest (pks ++ [pk]) (totalLen + parsedLen)
   in extract payloadLen rawPackets [] 0

-- returns parsed packet, the bitstring that remains after parsing it and the length of the bitstring that represent
-- the parsed packet
parsePacket :: Bitstring -> (Packet, Bitstring, Int)
parsePacket packet@(v0 : v1 : v2 : t0 : t1 : t2 : payload) =
  let version = [v0, v1, v2]
      typeID = [t0, t1, t2]
   in if getOperator typeID == Val
        then
          let (parsedLength, parsedPayload) = parseLiteralPayload payload
              totalPacketLength = 6 + parsedLength -- the header has a fixed length of 6
              padding = drop totalPacketLength packet
           in (Packet (Header version typeID) (Literal parsedPayload), padding, totalPacketLength)
        else
          let (lengthTypeID : rawPackets) = payload
           in if lengthTypeID
                then
                  let numOfSubPkts = bitstringToInt (take 11 rawPackets)
                      -- TODO: refactor then and else branches using a function that take another function
                      -- i.e. withNumber or withLength and how many bits to take 11 or 15 ...
                      packets = drop 11 rawPackets
                      (decodedPks, padding, parsedLen) = parsePacketsPayloadWithNumber numOfSubPkts packets
                      totalLen = 6 + 1 + 11 + parsedLen -- version + length type bit + length encoding + parsed length
                   in (Packet (Header version typeID) (Packets decodedPks), padding, totalLen)
                else
                  let lenOfSubPkts = bitstringToInt (take 15 rawPackets)
                      packets = drop 15 rawPackets
                      (decodedPks, padding, parsedLen) = parsePacketsPayloadWithLength lenOfSubPkts packets
                      totalLen = 6 + 1 + 15 + parsedLen -- version + length type bit + length encoding + parsed length
                   in (Packet (Header version typeID) (Packets decodedPks), padding, totalLen)
parsePacket bstr = error $ "Error! parsePacket - invalid bitstring: " ++ showBitstring bstr

-- returns the sum of the version numbers of this packet and all of its sub-packets
getVersionSum :: Packet -> Int
getVersionSum packet =
  let ver = bitstringToInt . version $ header packet
      pld = payload packet
   in case pld of
        Literal _ -> ver
        Packets packets -> ver + sum (map getVersionSum packets)

-- TODO: refactor all of these functions: use higher level functions and pass lower
-- level functions e.g. (+) (>) (==) etc etc
sumPackets :: [Packet] -> Int
sumPackets pks = sum $ map calculatePacketExp pks

multiplyPackets :: [Packet] -> Int
multiplyPackets pks = product $ map calculatePacketExp pks

getMinimumPacket :: [Packet] -> Int
getMinimumPacket pks = minimum $ map calculatePacketExp pks

getMaximumPacket :: [Packet] -> Int
getMaximumPacket pks = maximum $ map calculatePacketExp pks

isGtPacket :: [Packet] -> Int
isGtPacket (p0 : p1 : _) =
  let v0 = calculatePacketExp p0
      v1 = calculatePacketExp p1
   in if v0 > v1 then 1 else 0
isGtPacket _ = error "Error! isGtPacket - packet list should contains at least two packets"

isLtPacket :: [Packet] -> Int
isLtPacket (p0 : p1 : _) =
  let v0 = calculatePacketExp p0
      v1 = calculatePacketExp p1
   in if v0 < v1 then 1 else 0
isLtPacket _ = error "Error! isLtPacket - packet list should contains at least two packets"

areEqPackets :: [Packet] -> Int
areEqPackets (p0 : p1 : _) =
  let v0 = calculatePacketExp p0
      v1 = calculatePacketExp p1
   in if v0 == v1 then 1 else 0
areEqPackets _ = error "Error! areEqPackets - packet list should contains at least two packets"

-- TODO: refactor this! use functions to handle cases
calculatePacketExp :: Packet -> Int
calculatePacketExp packet =
  let op = getOperator $ typeID (header packet)
      pl = payload packet
   in case op of
        Sum -> case pl of
          Literal _ -> error "Error! calculatePacketExp - Corrupted packet: a Sum packet cannot contain a Literal value"
          Packets pks -> sumPackets pks
        Prod -> case pl of
          Literal _ -> error "Error! calculatePacketExp - Corrupted packet: a Prod packet cannot contain a Literal value"
          Packets pks -> multiplyPackets pks
        Min -> case pl of
          Literal _ -> error "Error! calculatePacketExp - Corrupted packet: a Min packet cannot contain a Literal value"
          Packets pks -> getMinimumPacket pks
        Max -> case pl of
          Literal _ -> error "Error! calculatePacketExp - Corrupted packet: a Max packet cannot contain a Literal value"
          Packets pks -> getMaximumPacket pks
        Val -> case pl of
          Literal v -> bitstringToInt v
          Packets _ -> error "Error! calculatePacketExp - Corrupted packet: a Val packet cannot contain sub-packets"
        Gt -> case pl of
          Literal _ -> error "Error! calculatePacketExp - Corrupted packet: a Gt packet cannot contain a Literal value"
          Packets pks -> isGtPacket pks
        Lt -> case pl of
          Literal _ -> error "Error! calculatePacketExp - Corrupted packet: a Lt packet cannot contain a Literal value"
          Packets pks -> isLtPacket pks
        Eq -> case pl of
          Literal _ -> error "Error! calculatePacketExp - Corrupted packet: a Eq packet cannot contain a Literal value"
          Packets pks -> areEqPackets pks

main = do
  message <- readFile "input.txt"

  -- some tests
  putStrLn "\nTesting hex string to bin string conversion:"
  putStrLn $
    "Test on input sample #1 - Passed: "
      ++ show (showBitstring (hexStringToBitstring inputSample1) == binStringSample1)
  putStrLn $
    "Test on input sample #2 - Passed: "
      ++ show (showBitstring (hexStringToBitstring inputSample2) == binStringSample2)
  putStrLn $
    "Test on input sample #3 - Passed: "
      ++ show (showBitstring (hexStringToBitstring inputSample3) == binStringSample3)

  putStrLn "\nTesting packet version and type ID:"
  let (samplePacket1, _, _) = parsePacket $ hexStringToBitstring inputSample1
  let sampleHeader1 = header samplePacket1
  let (samplePacketVersion1, sampleTypeID1) = (version sampleHeader1, typeID sampleHeader1)
  putStrLn $
    "Test on input sample #1 - Passed: "
      ++ show (bitstringToInt samplePacketVersion1 == 6 && bitstringToInt sampleTypeID1 == 4)
  let (samplePacket2, _, _) = parsePacket $ hexStringToBitstring inputSample2
  let sampleHeader2 = header samplePacket2
  let (samplePacketVersion2, sampleTypeID2) = (version sampleHeader2, typeID sampleHeader2)
  putStrLn $
    "Test on input sample #2 - Passed: "
      ++ show (bitstringToInt samplePacketVersion2 == 1 && bitstringToInt sampleTypeID2 == 6)
  let (samplePacket3, _, _) = parsePacket $ hexStringToBitstring inputSample3
  let sampleHeader3 = header samplePacket3
  let (samplePacketVersion3, sampleTypeID3) = (version sampleHeader3, typeID sampleHeader3)
  putStrLn $
    "Test on input sample #3 - Passed: "
      ++ show (bitstringToInt samplePacketVersion3 == 7 && bitstringToInt sampleTypeID3 == 3)

  putStrLn "\nTesting packet version sum:"
  let (samplePacket4, _, _) = parsePacket $ hexStringToBitstring inputSample4
  putStrLn $ "Test on input sample #4 - Passed: " ++ show (getVersionSum samplePacket4 == 16)
  let (samplePacket5, _, _) = parsePacket $ hexStringToBitstring inputSample5
  putStrLn $ "Test on input sample #5 - Passed: " ++ show (getVersionSum samplePacket5 == 12)
  let (samplePacket6, _, _) = parsePacket $ hexStringToBitstring inputSample6
  putStrLn $ "Test on input sample #6 - Passed: " ++ show (getVersionSum samplePacket6 == 23)
  let (samplePacket7, _, _) = parsePacket $ hexStringToBitstring inputSample7
  putStrLn $ "Test on input sample #7 - Passed: " ++ show (getVersionSum samplePacket7 == 31)

  -- solutions
  putStrLn $ "\nEvaluating message: " ++ message ++ "\n"
  let (parsedPacket, _, _) = parsePacket $ hexStringToBitstring message
  putStrLn $ "Part 1 - Version sum: " ++ show (getVersionSum parsedPacket)
  putStrLn $ "Part 2 - Packet value: " ++ show (calculatePacketExp parsedPacket)