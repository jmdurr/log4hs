# log4hs

### Overview
log4hs is a logging framework for haskell based on log4js.
Currently it supports:
 * Rolling logs
 * File logging
 * Async logging
 * Console logging
 * Pattern layouts

### LoggerT
A built in monad transformer that supports logging.

### HasLog m l
A class whose instances are capable of logging. The logMsg functions occur in the m monad and the output occurs in the l monad

### Logging.Log4hs.TH.Log4hs
Template haskell with logging functions that display the module that the logging message was sent from. This also enables setting different log settings for different modules in the logconfig.

### Pattern Layouts
log4hs supports pattern layouts similar to log4j. The options are:

%d{*dateformat string*}
> Displays the date given a format string understood by [Data.Time.Format](http://hackage.haskell.org/package/time/docs/Data-Time-Format.html)  
  

%encode{*CRLF|JSON*}{*sub-pattern*}
%enc{*CRLF|JSON*}{*sub-pattern*}
> Escapes the result of sub-pattern to, CRLF will escape newlines. JSON will escape the data to be suitable for use as a json string.

%highlight{*sub-pattern*}{*opts*}
> Highlights the result of the sub-pattern given the opts.
> Opts are [(LogLevel,Color)] where LogLevel is one of 
> * [ERROR,WARN,INFO,TRACE,DEBUG]<br/>
> and color is one of 
> * [white,black,red,magenta,blue,green,yellow,cyan]


%maxLength{*sub-pattern*}{*int*}
%maxLen{*sub-pattern*}{*int*}
> Safely truncates the result of the sub-pattern to be no more than *int* characters long

%pid
%processId
> Prints the current process id

%p{LOGLEVEL=ALIAS,LOGLEVEL=ALIAS}
%level{LOGLEVEL=ALIAS,LOGLEVEL=ALIAS}
> Prints the level at which the entry was made. If the LOGLEVEL=ALIAS pairs are provided ex: (WARN=!!) then the level will be printed as the alias

%t
%threadId
%tid
> Prints the current thread id

%%
> Prints '%'

%message
%msg
%m
> Prints the log message passed to logMsg

%K{key}
%map{key}
%MAP{key}
> Prints the value of the name value pairs passed in to logMsg where the key matches


