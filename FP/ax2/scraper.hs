{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.LaTeX
import Data.List
import Data.Function
import Data.List.Split
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
    let universityURL = "http://www.gla.ac.uk/schools/computing/staff/"
    researchPersonsList <- scrapeURL universityURL scrapeResearchList
    managementPersonsList <- scrapeURL universityURL scrapeManagementList
    affiliatePersonsList <- scrapeURL universityURL scrapeAffiliateList
    honoraryPersonsList <- scrapeURL universityURL scrapeHonoraryList
    let validResearchPersonsList = fromMaybe [] researchPersonsList
    let validManagementPersonsList = fromMaybe [] managementPersonsList
    let validAffiliatePersonsList = fromMaybe [] affiliatePersonsList
    let validHonoraryPersonsList = fromMaybe [] honoraryPersonsList
    let fullValidList = (validResearchPersonsList !! 0) ++ (validManagementPersonsList !! 0) ++ (validAffiliatePersonsList !! 0) ++ (validHonoraryPersonsList !! 0)
    let l = sortBy compare fullValidList
    let noDuplicatesList = removeDuplicates l
    contacts <- mapM scrapeContactURL noDuplicatesList
    let contactsWithPhoneNumbers = filter removePeopleWithoutNumbers contacts
    let finalContacts = map removePhoneNumberList contactsWithPhoneNumbers
    let ccc = filter (\(x,y) -> (isInfixOf "telephone" y)) finalContacts
    let fff = map getPhoneNumberFromString ccc
    let r = fff
    text <- execLaTeXT (buildHatex r)
    renderFile "directory.tex" text

scrapeResearchList :: Scraper String [[(String, String)]]
scrapeResearchList = 
    chroots ("div" @: ["id" @= "research-teaching"]) scrapeUl

scrapeManagementList :: Scraper String [[(String, String)]]
scrapeManagementList =
    chroots ("div" @: ["id" @= "management-support"]) scrapeUl

scrapeAffiliateList :: Scraper String [[(String, String)]]
scrapeAffiliateList =
    chroots ("div" @: ["id" @= "affiliate"]) scrapeUl

scrapeHonoraryList :: Scraper String [[(String, String)]]
scrapeHonoraryList =
    chroots ("div" @: ["id" @= "honorary-visiting"]) scrapeUl

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

scrapeContactURL (x,y) = do
    phoneNumber <- scrapeURL y scrapePerson
    let validPhoneNumber = fromMaybe [""] phoneNumber
    return (x, validPhoneNumber)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

removePeopleWithoutNumbers (a, b) 
    | (null b) = False
    | otherwise = True

removePhoneNumberList :: (String, [String]) -> (String, String)
removePhoneNumberList (x,y) = (x, (head y))

getPhoneNumberFromString :: (String, String) -> (String, String)
getPhoneNumberFromString (x,y) = (x, (splitOn " " (head (filter (\x -> (isInfixOf "telephone" x)) (splitOn "\n" y)))) !! 1)

buildHatex :: Monad m => [(String, String)] -> LaTeXT_ m
buildHatex sortedStaffProfiles = do
    documentclass [] article
    author "Andrei-Mihai Nicolae (2147392n)"
    title "Telephone Directory"
    document (texBody sortedStaffProfiles)

texBody :: Monad m => [(String, String)] -> LaTeXT_ m
texBody sortedStaffProfiles = do
    maketitle
    bigskip
    center $ textbf "Contacts"
    mapM_ (\(name, phone) -> do
        textbf (fromString name)
        hfill
        fromString phone
        lnbk) sortedStaffProfiles
