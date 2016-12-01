{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.LaTeX
import Data.Function
import Data.List
import Data.Maybe

main :: IO ()
main = do
    personsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeList 
    case personsList of
        Just list -> do
            let l = sortBy compare list
            let urls = map (\(x,y) -> y) (head l)
            let names = map (\(x,y) -> x) (head l)
            let contacts = map scrapeURLs (head l)
            mapM_ (\x -> (x>>=(print))) contacts
        Nothing -> print "Text could not be retrieved!"

scrapeList :: Scraper String [[(String, String)]]
scrapeList = 
    chroots ("div" @: ["id" @= "research-teaching"]) scrapeUl

scrapeUl :: Scraper String [(String, String)]
scrapeUl =
    chroots "li" scrapeLi

scrapeLi :: Scraper String (String, String)
scrapeLi = do
    personName <- text $ "a"
    url <- attr "href" $ "a"
    let fullurl = "http://www.gla.ac.uk" ++ url
    return (personName, fullurl)

scrapePerson :: Scraper String [String]
scrapePerson = 
    chroots ("div" @: ["id" @= "sp_contactInfo"]) scrapeNumber

scrapeNumber :: Scraper String String
scrapeNumber = do
    num <- text $ "p"
    return num

scrapeURLs (x,y) = do
    phoneNumber <- scrapeURL y scrapePerson
    return (x, phoneNumber)
