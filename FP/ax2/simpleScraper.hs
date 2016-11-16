{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel

type Name = String
data Person 
    = PersonInfo Name URL
    deriving (Show, Eq)

main :: IO ()
main = do
    personsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeList
    print personsList

scrapeList :: Scraper String [[Person]]
scrapeList = 
    chroots ("div" @: ["id" @= "research-teaching"]) scrapeUl

scrapeUl :: Scraper String [Person]
scrapeUl =
    chroots "li" scrapeLi

scrapeLi :: Scraper String Person
scrapeLi = do
    personName <- text $ "a"
    personURL  <- attr "href" $ "a"
    return $ PersonInfo personName personURL
