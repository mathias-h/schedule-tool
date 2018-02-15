
module Main where
    import qualified Data.Text as T
    import Text.Regex()
    import Data.List
    import System.Process
    import Data.Maybe
    import Data.Dates
    import Data.Either

    findSubstringIndex sub str = findIndex (isPrefixOf sub) (tails str) 

    getHtml url = readCreateProcess (proc "./curl" [url]){ std_out = CreatePipe, cwd = Just "/Users/mathias/Documents/code/skema" } ""

    getCourseUrl id = "https://skemasys.akademiaarhus.dk/index.php?educationId=1&menuId=1&account=timetable_subject&subjectId=" ++ show id

    getCourse id =
        let courseUrl = getCourseUrl id
            getTables html = map ((\s -> take (fromJust $ findSubstringIndex "</table>" s) s) . T.unpack) $ drop 1 $ T.splitOn (T.pack "<table id=\"calendar\"") (T.pack html)
            getDate now s = fromRight (DateTime 1 1 1 1 1 1) $ parseDate now $ (drop 6 s) ++ "/" ++ ((take 2 . drop 3) s) ++ "/" ++ (take 2 s)
            getCourseName l = 
                let name = T.strip $ T.pack (drop 34 $ take (fromJust $ elemIndex '<' l) l)
                in if name == T.pack "" then Nothing else Just name
            getLessons now table = map ((\l -> (getDate now (take 10 l), getCourseName l)) . T.unpack) $ filter (\l -> (take 4 . drop 15) (T.unpack l) == "2433") $ T.splitOn (T.pack "<td id=\"date_") (T.pack $ table)
            isInThisWeek now d = d >= (minusInterval (lastMonday now) (Days 1)) && d < (nextMonday now { hour = 0, minute = 0, second = 0})
        in do
            html <- getHtml $ getCourseUrl id
            now <- getCurrentDateTime
            return $ filter (\(d,_) -> isInThisWeek (addInterval now (Days 2)) d) (foldr (++) [] $ map (getLessons (addInterval now (Days 2))) $ getTables html)

    getCourses = 
        let coursesUrl = "https://skemasys.akademiaarhus.dk/index.php?educationId=1&menuId=1&account=timetable_semester&semesterId=569"
            getRows html = map (T.splitOn $ T.pack "&nbsp;") . tail . T.splitOn (T.pack "<tr id=\"subject_") . T.pack $ html
            getSubjectId s = read $ take (fromJust $ elemIndex '"' s) s :: Int
            getSubjectName s = T.strip . T.pack . (take $ fromJust (elemIndex '<' s)) $ s
            getValues = map ((\(a:b:_) -> (getSubjectId $ T.unpack a, getSubjectName $ T.unpack b)) . take 2)
        in do
            html <- getHtml coursesUrl
            return $ getValues . getRows $ html

    showCourse i courses
        | i < length courses = do 
            putStrLn $ "(" ++ (show i) ++ ") " ++ (T.unpack $ snd (courses !! i))
            showCourse (i+1) courses
        | otherwise = return ()
    displayCourses courses = showCourse 0 courses

    selectCourse courses selected
        | (length selected) < 3 = do
            input <- getLine
            if (read input :: Int) < 0 || (read input :: Int) >= (length courses)
                then do
                    putStrLn "index invalid"
                    selectCourse courses selected
                else
                    selectCourse courses ((courses !! (read input :: Int)) : selected)
        | otherwise = return selected
    selectCourses courses = selectCourse courses []

    selectLesson ((d,a),(_,b),(_,c))
        | isJust a = (d,a)
        | isJust b = (d,b)
        | isJust c = (d,c)
        | otherwise = (d,Nothing)
    getSchedule lessons =
        map selectLesson $ zip3 (lessons !! 0) (lessons !! 1) (lessons !! 2 )

    padString s l = padding ++ s ++ padding ++ (take (mod (length s-l) 2) (repeat ' '))
        where padding = (take (quot (l - length s) 2 + 2) (repeat ' '))
    padStrings (a,b) = 
        (padString a l, padString b l)
        where l = max (length a) (length b)
    showSchedule schedule =
        let showDate d = (show $ day d) ++ "/" ++ (show $ month d)
            getWeekDay s = (show $ dateWeekDay s) ++ " " ++ (showDate s)
            getCourseName t = words (T.unpack $ fromMaybe (T.pack "Nothing") t) !! 0
            weekdayCoursePairs = map (padStrings . (\(a,b) -> (getWeekDay a, getCourseName b))) schedule
            displayStrings ss = putStrLn $ "|" ++ (intercalate "|" $ ss) ++ "|"
        in do
            displayStrings $ map fst weekdayCoursePairs
            displayStrings $ map snd weekdayCoursePairs
            return ()
            
    -- TODO check for late classes
    -- TODO select weeks

    main = do
        courses <- getCourses
        displayCourses courses
        selectedCourses <- selectCourses courses
        lessons <- mapM getCourse $ map fst selectedCourses
        showSchedule $ getSchedule lessons
        return ()
