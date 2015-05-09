module Handler.Differentiate where

import Import as I
import Prelude
import Data.Text as T
import Data.List as L
import Data.Char as C

generatePoly :: Form Poly
generatePoly = renderTable $ Poly
    <$> areq textField "Equation: " Nothing

derivative :: Poly -> String 
derivative poly = 
            let things = L.filter (\x -> x=="+" || x=="-") $ L.map T.unpack $ parsePoly poly
            in  finalClean I.. showZip $ I.zip (L.map singleDer $ clean $ parsePoly poly) ("x":things)

clean :: [Text] -> [Text]
clean textList = L.filter (\x -> x /= T.empty && x /= T.pack "+" && x /= T.pack "-") $ L.map checkText textList

showZip :: [(String, String)] -> String
showZip values = L.foldl (\acc tuple -> acc I.++ (I.fst tuple I.++ " " I.++ I.snd tuple I.++ " ")) "" values

checkText :: Text -> Text
checkText text = 
                if not ('x' `I.elem` T.unpack text) then
                    T.pack ""
                else if not (C.isDigit $ T.head text) then
                    T.append (T.pack "1") text
                else if not ('^' `I.elem` T.unpack text) then
                    T.append text $ T.pack "^1"
                else
                    text

parsePoly :: Poly -> [Text]
parsePoly (Poly equation) = T.words equation

singleDer :: Text -> String
singleDer part = 
    let first  = read (L.takeWhile (/='x') $ T.unpack part) :: Int
        second = read (L.reverse $ L.takeWhile (/='^') $ L.reverse $ T.unpack part) :: Int
    in  show (first * second) I.++ "x" I.++ "^" I.++ show (second - 1)

getEqu :: Poly -> Text
getEqu (Poly equ) = equ

finalClean :: String -> String
finalClean string = L.reverse $ I.snd $ L.span (\x -> not $ C.isDigit x) $ L.reverse string

getDifferentiateR :: Handler Html
getDifferentiateR = do
    (widget, enctype) <- generateFormPost generatePoly
    defaultLayout 
        [whamlet|
            <p>Enter your polynomial here:
            <form action=@{DifferentiateR} method=post enctype=#{enctype}>
                <table>
                    ^{widget}
                <button>Submit
        |]

postDifferentiateR :: Handler Html
postDifferentiateR = do
    ((result, widget), enctype) <- runFormPost generatePoly
    case result of
        FormSuccess poly -> do
                            defaultLayout 
                                [whamlet| 
                                    Original: #{show $ getEqu poly} </br>
                                    Derivative: #{show $ derivative poly} </br>
                                    <a href=@{DifferentiateR}>Return
                                |]
        _ -> do
            defaultLayout [whamlet|
                <p>Sorry. Seems like a strange input. Try again.
                <form action=@{DifferentiateR} method=post enctype=#{enctype}>
                    <table>
                        ^{widget}
                        <button>Submit
            |]