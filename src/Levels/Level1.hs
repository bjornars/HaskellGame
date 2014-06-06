module Levels.Level1 (level) where

import Level
import Types
import Actor.Zombie


bluePrint :: [String]
bluePrint =
    ["XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ,"X........................................XXXXXXXXXXXXXXX...............................XXXXX"
    ,"X....................................@...XXXXXXXXXXXXXXX...............................XXXXX"
    ,"X........................................XXXXXXXXXXXXXXX...............................XXXXX"
    ,"X........................................XXXXXXXXXXXXXXX...............................XXXXX"
    ,"X....XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.SSSSS...........XXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.SSSSS...........XXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XXXXXXXXXXXXXXXXXXX...................XXXXXXXXXXXXX.....XXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XXXXXX.........XXXX....................XXXXXXXXXXXX.....XXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.....XXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX.................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX.................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX............T.XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX..............XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX............T.XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX................SSSS.............XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX......XXXXX.....SSSS.............XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX......XXXXX.....SSSS.............XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX......XXXXX......................XXXXXXXXXXXXXXXXXXXXXX.........................XXXXX"
    ,"X....XX.................................XXXXXXXXXXXXXXXXXXXXXX.........................XXXXX"
    ,"X....XXXXXX.............................XXXXXXXXXXXXXXXXXXXXXX.........................XXXXX"
    ,"X....XXXXXX.............................XXXXXXXXXXXXXXXXXXXXXX.........................XXXXX"
    ,"X....XXXXXX.............................XXXXXXXXXXXXXXXX...............................XXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX...............................XXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX...............................XXXXX"
    ,"X...............................XXXXXXXXXXXXXXXXXXXXXXXX...............................XXXXX"
    ,"X...............................XXXXXXXXXXXXXXXXXXXXXXXX...............................XXXXX"
    ,"X....XXXXXX.........XXXXX..............................................................XXXXX"
    ,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ]


-- level :: Monster m => (Level, Hero, [m])
level = loadLevel bluePrint [('S', mkZombie)]
