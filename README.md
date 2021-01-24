# EchoBot

This bot sends messages back to telegram and vk
Bot can  repeat last message and if you need to change count repeate 

                /repeat

If you want to get help message 

                 /help



## Configuration:
  All configuration are in the src/Config.hs and you need to write in your customization or if need I can rewrite project with change "bot.config"
        help message
        count repeat message
        log configuration ( logFile - where to write?, logLevelForFile - recording level(Debug, Warning ,Error), logConsole  - is write to console?)
        and some specific option for every bot just need only for administrator

## For start:
   for start bot you need to create file "bot.config" like file from folder templates, to replaced "token" with your token

##  How to use

### All you need to start project:

*       create file 
                
                "bot.config" 

        like in 
                
                templates/bot.config.template 

*      write your option and script to start project, like this:
        
                TelegramToken 889:9jhhjj
                VKtoken dsfdsfsdfsadfasfasddsfsd345
                RepeatCount 2
                HelpMessage "default help message"

*       take script in terminal:

                stack build
                
## Structure of project.
        
        All logic divided for 2 group: Bot  (main logic) and Adapter (implementation). 
        I used for ReaderT design pattern. 
        Adapter/Telegram - all logic for Telegram.
        Adapter/VK - all logic for VK.
        Keyboard VK is in keyboard.json
        log-journalTest and  log-journal - files for write log in testing and running programm
        Config - default option  



## Test.
I used spec library for testing app and empty structure.
