{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel

type Name = String
type TelNumber = String
data Person 
    = PersonInfo Name TelNumber
    deriving (Show, Eq)

getUrl :: [[Person]] -> String
getUrl (PersonInfo _ number) = number

main :: IO ()
main = do
    personsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeList
    numbers <- scrapeURL  scrapePerson
    print personsList
    print numbers

scrapeList :: Scraper String [[Person]]
scrapeList = 
    chroots ("div" @: ["id" @= "research-teaching"]) scrapeUl

scrapeUl :: Scraper String [Person]
scrapeUl =
    chroots "li" scrapeLi

scrapeLi :: Scraper String Person
scrapeLi = do
    personName <- text $ "a"
    personNumber <- attr "href" $ "a"
    return $ PersonInfo personName personNumber

scrapePerson :: Scraper String [TelNumber]
scrapePerson = 
    chroots ("div" @: ["id" @= "sp_contactInfo"]) scrapeNumber

scrapeNumber :: Scraper String TelNumber
scrapeNumber = do
    num <- text $ "p"
    return num
