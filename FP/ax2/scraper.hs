-- Name: Andrei-Mihai Nicolae
-- Email address: 2147392n@student.gla.ac.uk

-- Web scraper that creates a telephone directory from the Glasgow University
-- Computing Science department website; it uses the Scalpel web framework and
-- outputs the records using the HaTeX open source hackage. Everything works as
-- expected, capturing all people with their phone numbers correctly. It also
-- handles the case when sp_contactInfo div does not exist, but the phone number
-- is "hiding" inside a simple paragraph. It always runs in ~5 minutes as the
-- specification did not require a multi-threaded version.

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

-- main function that will call all the functions defined below and will create
-- the tex file containing all the people in the Computing Science department
-- who have a name; the output will be sorted, will have all duplicates removed
-- and will contain all the people from all tabs (i.e. research & teaching, management
-- and support, affiliate and honorary); everything will be, in the end, formatted and
-- outputted into directory.tex
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
    let fullValidList = (validResearchPersonsList !! 0) ++ (validManagementPersonsList !! 0) ++ 
                        (validAffiliatePersonsList !! 0) ++ (validHonoraryPersonsList !! 0)
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
    print "---> Computed final contacts! Starting to write them to tex file..."
    body <- execLaTeXT (constructDocument records)
    renderFile "directory.tex" body
    print "---> Program finished executing! Output is located inside directory.tex."

-- scraper that scrapes the url for a div with the id research-teaching
-- used for getting all the research and teaching people in the department
scrapeResearchList :: Scraper String [[(String, String)]]
scrapeResearchList = 
    chroots ("div" @: ["id" @= "research-teaching"]) scrapeUl

-- scraper that scrapes the url for a div with the id management-support
-- used for getting all the management and support people in the department
scrapeManagementList :: Scraper String [[(String, String)]]
scrapeManagementList =
    chroots ("div" @: ["id" @= "management-support"]) scrapeUl

-- scraper that scrapes the url for a div with the id affiliate
-- used for getting all the affiliate people in the department
scrapeAffiliateList :: Scraper String [[(String, String)]]
scrapeAffiliateList =
    chroots ("div" @: ["id" @= "affiliate"]) scrapeUl

-- scraper that scrapes the url for a div with the id honorary-visiting
-- used for getting all the honorary and visiting people in the department
scrapeHonoraryList :: Scraper String [[(String, String)]]
scrapeHonoraryList =
    chroots ("div" @: ["id" @= "honorary-visiting"]) scrapeUl

-- scraper that scrapes inside a div to get the li element which will
-- contain the list of people
scrapeUl :: Scraper String [(String, String)]
scrapeUl =
    chroots "li" scrapeLi

-- scraper that gets the person name and url from the a element
-- the text is the name, and the href attribute is the url to his/her
-- personal page; we need to check if the url contains the computing/staff
-- suffix - if not, we need to append it as for some people the url is in the
-- form of ?action=person..., thus giving us an invalid address
scrapeLi :: Scraper String (String, String)
scrapeLi = do
    personName <- text $ "a"
    url <- attr "href" $ "a"
    if (isInfixOf "/computing/staff" url) then return (personName, "http://www.gla.ac.uk" ++ url)
        else return (personName, "http://www.gla.ac.uk/schools/computing/staff/" ++ url)

-- first out of two scrapers that get the phone number out of a page;
-- this is the one where the person's personal page has a div with the id
-- sp_contactInfo
scrapePerson :: Scraper String [String]
scrapePerson = 
    chroots ("div" @: ["id" @= "sp_contactInfo"]) scrapeNumber

-- second scraper for getting the phone number: this is used when the person
-- does not have an sp_contactInfo div; if so, we look for a paragraph inside
-- the mainContent div with a certain style and retrieve the number from there 
scrapePersonTwo :: Scraper String [String]
scrapePersonTwo =
    chroots ("div" @: ["id" @= "mainContent"]) scrapeNumberTwo

-- this gets the actual number from inside the paragraph inside the mainContent div
scrapeNumberTwo :: Scraper String String
scrapeNumberTwo = do
    number <- text $ "p" @: ["style" @= "margin: 0 0 10px 25px; padding: 5px; color: ##333;"]
    return number

-- this gets the number from the paragraph when we have an sp_contactInfo div
scrapeNumber :: Scraper String String
scrapeNumber = do
    num <- text $ "p"
    return num

-- function that gets as input the name of a person and its personal url
-- and then scrapes that address using the 2 above-mentioned scrapers; if the first
-- one gets some output, then we return a tuple (name, number); if not, then we
-- retrieve the name and the number taken from the mainContent div; we also used
-- fromMaybe in order to work with Just values
scrapeContactURL :: (String, URL) -> IO (String, [String])
scrapeContactURL (x,y) = do
    phoneNumber <- scrapeURL y scrapePerson
    phoneNumberTwo <- scrapeURL y scrapePersonTwo
    let validPhoneNumber = fromMaybe [""] phoneNumber
    let validPhoneNumberTwo = fromMaybe [""] phoneNumberTwo 
    if (length (validPhoneNumber) /= 0) then return (x, validPhoneNumber)
        else return (x, validPhoneNumberTwo)

-- function that sorts and removes all the duplicates from the full list
-- of people retrieved from all tabs
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

-- function that used to filter out people with empty personal information
removePeopleWithoutNumbers (a, b) 
    | (null b) = False
    | otherwise = True

-- function that is mapped in the main function in order to retrieve
-- only the head of the personal information list (i.e. that contains the 
-- phone number)
removePhoneNumberList :: (String, [String]) -> (String, String)
removePhoneNumberList (x,y) = (x, (head y))

-- POSIX regex that is used to retrieve only the phone number from inside
-- the whole paragraph containing that person's email, telephone etc. 
getPhoneNumberFromString :: (String, String) -> (String, String)
getPhoneNumberFromString (x,y) = (x, (y =~ ("[ +()]*[0-9][ +()0-9]*" :: String)))

-- function that creates the basic info for the LaTeX document and calls
-- the constructDocumentBody function to write all the records collected previously
constructDocument :: Monad m => [(String, String)] -> LaTeXT_ m
constructDocument records = do
    documentclass [] article
    author "Andrei-Mihai Nicolae (2147392n)"
    title "Telephone Directory"
    document (constructDocumentBody records)

-- function that nicely outputs all phone directory records to the tex file
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