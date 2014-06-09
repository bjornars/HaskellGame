module Levels.Level1 (level) where

import Level
import Types
import Actor.Zombie


blueprint :: [String]
blueprint =
    ["XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ,"X........................................XXXXXXXXXXXXXXX...............................XXXXX"
    ,"X....................................@...XXXXXXXXXXXXXXX...............................XXXXX"
    ,"X........................................XXXXXXXXXXXXXXX...............................XXXXX"
    ,"X........................................XXXXXXXXXXXXXXX...............................XXXXX"
    ,"X....XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.ZZZZZ...........XXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.ZZZZZ...........XXXXXXXXXXXXXXXX..........XXXXX"
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
    ,"X....XX................ZZZZ.............XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX......XXXXX.....ZZZZ.............XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
    ,"X....XX......XXXXX.....ZZZZ.............XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........XXXXX"
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


level :: (Level, Hero, Monsters)
level = loadLevel blueprint [('Z', mkZombie)]
