{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.LaTeX
import Text.Regex
import Text.Regex.Base
import Text.Regex.Posix
import Data.List
import Data.Function
import Data.List.Split
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
    let universityURL = "http://www.gla.ac.uk/schools/computing/staff/"
    print "---> Starting to retrieve research and teaching persons list..."
    researchPersonsList <- scrapeURL universityURL scrapeResearchList
    print "---> Research and teaching persons list retrieved! Starting to retrieve management and support persons list..."
    managementPersonsList <- scrapeURL universityURL scrapeManagementList
    print "---> Management and support persons list retrieved! Starting to retrieve affiliate persons list..."
    affiliatePersonsList <- scrapeURL universityURL scrapeAffiliateList
    print "---> Affiliate persons list retrieved! Starting to retrieve honorary persons list..."
    honoraryPersonsList <- scrapeURL universityURL scrapeHonoraryList
    print "---> Honorary persons list retrieved! Starting to sort and remove duplicates from the full list of people..."
    let validResearchPersonsList = fromMaybe [] researchPersonsList
    let validManagementPersonsList = fromMaybe [] managementPersonsList
    let validAffiliatePersonsList = fromMaybe [] affiliatePersonsList
    let validHonoraryPersonsList = fromMaybe [] honoraryPersonsList
    let fullValidList = (validResearchPersonsList !! 0) ++ (validManagementPersonsList !! 0) ++ (validAffiliatePersonsList !! 0) ++ (validHonoraryPersonsList !! 0)
    let sortedAndNoDuplicatesList = removeDuplicates fullValidList
    print  "---> Sorting and removing duplicates completed! Starting to append corresponding phone numbers to all people..."
    contacts <- mapM scrapeContactURL sortedAndNoDuplicatesList
    print "---> All phone numbers retrieved! Starting to perform filtering and removing people with incorrect phone numbers..."
    let peopleWithoutEmptyInfo = filter removePeopleWithoutNumbers contacts
    let contactsWithHeadOfList = map removePhoneNumberList peopleWithoutEmptyInfo
    let contactsWithTelephone = filter (\(x,y) -> (isInfixOf "telephone" y)) contactsWithHeadOfList
    print  "---> Filtering and removing redundant information completed! Starting to retrieve only the phone number without any other text..."
    let finalContacts = map getPhoneNumberFromString contactsWithTelephone
    let records = finalContacts
    body <- execLaTeXT (constructDocument records)
    renderFile "directory.tex" body

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

scrapePersonTwo :: Scraper String [String]
scrapePersonTwo = do
    content <- texts $ "p" @: ["style" @= "margin: 0 0 10px 25px; padding: 5px; color: ##333;"]
    return content

scrapeNumber :: Scraper String String
scrapeNumber = do
    num <- text $ "p"
    return num

scrapeContactURL :: (String, URL) -> IO (String, [String])
scrapeContactURL (x,y) = do
    phoneNumber <- scrapeURL y scrapePerson
    let validPhoneNumber = fromMaybe [""] phoneNumber
    if (validPhoneNumber /= []) then return (x, validPhoneNumber)
        else do
            secondPhoneNumber <- scrapeURL y scrapePersonTwo
            let validSecondPhoneNumber = fromMaybe [""] secondPhoneNumber
            return (x, validSecondPhoneNumber)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

removePeopleWithoutNumbers (a, b) 
    | (null b) = False
    | otherwise = True

removePhoneNumberList :: (String, [String]) -> (String, String)
removePhoneNumberList (x,y) = (x, (head y))

getPhoneNumberFromString :: (String, String) -> (String, String)
getPhoneNumberFromString (x,y)  = (x, (y =~ ("[ +()]*[0-9][ +()0-9]*" :: String)))

constructDocument :: Monad m => [(String, String)] -> LaTeXT_ m
constructDocument records = do
    documentclass [] article
    author "Andrei-Mihai Nicolae (2147392n)"
    title "Telephone Directory"
    document (constructDocumentBody records)

constructDocumentBody :: Monad m => [(String, String)] -> LaTeXT_ m
constructDocumentBody records = do
    maketitle
    bigskip
    center $ textbf "Records"
    mapM_ (\(name, phone) -> do
        textbf (fromString name)
        hfill
        fromString phone
        lnbk) records
