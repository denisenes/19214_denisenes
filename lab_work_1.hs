--1--
myGet :: [a] -> Integer -> a
myGet [] _ = error "Empty list"
myGet (x:_) 0 = x
myGet (_:[]) _ = error "Out of bounds"
myGet (_:xs) n = myGet xs (n - 1)

--2--
myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:_) = x

--3--
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

--4--
myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (_:xs) = xs

--5--
myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit [x] = []
myInit (x:xs) = x:(myInit xs)

--6--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = helper xs [] where
    helper [] ys = ys
    helper (x:xs) ys = helper xs (x:ys)

--7--
myLength :: [a] -> Integer
myLength xs = helper 0 xs where
    helper acc [] = acc
    helper acc (x:xs) = helper (acc + 1) xs

--8---
myAppend :: [a] -> a -> [a]
myAppend [] y = [y]
myAppend (x:xs) y = (x:(myAppend xs y))

--9---
myConcat :: [a] -> [a] -> [a]
myConcat xs ys = helper xs ys where
    helper [] yss = yss
    helper (x:xss) yss = x:(helper xss yss)

--10--
myDrop :: Integer -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = error "Out of bounds"
myDrop n (_:xs) = myDrop (n - 1) xs

--11--
myTake :: Integer -> [a] -> [a]
myTake n xs = helper n xs where
    helper 0 _ = []
    helper nn (x:xss) = x:(helper (nn-1) xss)

--12--
mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt _ [] = error "Empty list"
mySplitAt n xs = (myTake n xs, myDrop n xs)

--13--
myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

--14--
myElem :: (Eq a) => [a] -> a -> Bool
myElem [] _ = False 
myElem (x:xs) y = if x == y then True else myElem xs y

--15--
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = helper f (myReverse xs) [] where
    helper _ [] ys = ys
    helper ff (x:xss) ys = if (ff x) == True then helper ff xss (x:ys) else helper f xss ys

--16--
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x) : myMap f xs

--17--
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

{-                                                                                                                                                    
                                                                         ````                                                                         
                                                               .:+syhhddddmmNNmdhs+:-`                                                                
                                                         ./++++/:-.```      ``.....-/syyso/.                                                          
                                                    `:+s+-`                             `.:+++/-`                                                    
                                                  :++/.                                       `-+ss:                                                  
                                               `+o:`                                              -+y+`                                               
                                             `oo.            -------------------------               `.os:                                             
                                           -sd-              Me after haskell homework                 .sd-                                           
                                         `oNd.               -------------------------                   -h:                                          
                                        .dMy`                                                             `o/                                         
                                       :NM+                                                                 /s.                                       
                                      -NN/                                                                   ym+`                                     
                                     .mN:                                                                     +Ms                                     
                                     hM+                                    `---:://::/:.``                    sd                                     
                                    +Md                 .-.-/syhddys+o+//++osos+/++ososs+//::--:/o+/.          `m:                                    
                                   `NM/                       ``--`` `  .-:ooosoo++//+/:-/:`..` ```             /N-                                   
                                   sMm                  ```    ``````` `.sooyso+s+-....:/+:.-.`.` ``             sd`                                  
                                  .NM+             ```-sddhhdydmdddhyhhoohdhmd.        ohdssdmmmdddhdso/+.       `yo                                  
                                  sNh`             `:ydmdMMMMMNmNmdmmNNMMMNmdy`    `   oymMMMMmmmmdydhNMNy+-      /N                                  
                                 :Nm`                `.`:MMm//---``.-:/+yNy/``    `+     sMMdo/--.``.`:od.`       -M`                                 
                                 hMs                `+o+hMmoo+:.      ..sMs-     `+y     hNy+       .:-`s:        -M                                  
                                 dM/                 `sMMMm+dyo/      `.oMy`     :os     dN+o-`     shh-y/        /d                                  
                                `NM`             `-:/+yMMMNo+-.`  ```-`:ddo      -os.    :Nddo-`.   `::+N+``      +s                                  
                                +Md              ```...hMMMNmooo-.+/:ydNd/-      `sho     +NNNdy+:/osdmd+-:-      y:                                  
                                yMo                    :dMMNdmNNNmmmdNNy/:.      -oms.`    -yMMMMNNdmmdo`` .     `N`                                  
                                sM:                  ---sNNdmhhdmmNdsy//. .::.  `:dMMs:.  ``-MMMNdhyshs:``       /m                                   
                                oM-                      :sydhhyyddy+:`` /Nm+-/yyyNMMNhs:://oMhsssyo+-. ````.`   oy                                   
                                /M/      ./++::-::-.`      `-/+///-``-oy:NMmysmmmddmNNMMmmNNNh..`      `-...-::  +o                                   
                                `mh    `+y+.       ``      ```--/:--sNmNNNMmNMmmddNyo:+oshMds/sys-.`          :`.s:                                   
                                 -N+   s+          `.:-:+oosyshydNNNMMMMmysssooossy+++++//ssyhhdmmmds+//::`   .`:m`                                   
                                  :N: -y      .+/.+ooosos+o/ssshNMMMMMMMmhsoy++///+oosoo+::///::yddNhmhsos+/-s.-md                                    
                                   sm:`o     +NNdhds:------..``.:/ydmNMNdo/oo+yoo+ydyss//::/+ossoo:/::-:/ohh/N+/M+                                    
                                   `sm/.`    +N/+:-`                `-+sydmdhyddyysyoshyhhyys+-.        `.:+/o:sy                                     
                                     :do     :yso/.                      `.--omhhhhmdhy-.```           `+hdo:`os`                                     
                                      `+s-`    -oso.                      .os/:+://:....              -+:`` `yo`                                      
                                        -hh/`    `--`                      .-/++o+sso                `.    -s:                                        
                                          /hh/`                            `-  `` `od-                   .so.                                         
                                           `-sh/`                                 `/h-                 `:s+                                           
                                              -oyo:`                                o                .ohs                                             
                                                `-ohs:`                            `/              .yNy-                                              
                                                    `:++:.`                        `           `-odNm:                                                
                                                        -oyys+:-`                           -oymNy+-                                                  
                                                           `-/oyddhs+-.``               .+shhs/-                                                      
                                                               ```.-+oyhmhhyyssssssysso+/-`                                                           
                                                                          `.---.```.                                                                  
                                                                                                                                                      -}
