// This unit contain all messages
unit ftpmessages;

// This file is part of the Portabl and Pascal FTP Server
// Copyright (c) 2010 MARTINEAU Emeric.
//
// See the file license, included in this distribution,
// for details about the license.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

const
    // MESSAGE
    MSG_LOG_READ_CONFIG : String = 'Read configuration' ;
    MSG_LOG_SERVER_START : String = 'Server start at %s on port %d' ;
    MSG_LOG_SHUTDOWN : String = 'Server shutdown' ;
    MSG_LOG_NEW_CONNECTION : String = '[%s] New connection' ;
    MSG_LOG_CONNECTION_CLOSE : String = '[%s] Connection close' ;
    MSG_LOG_COMMAND : String = '[%s] %s' ;
    MSG_LOG_ANONYMOUS_PASSWORD : String = '[%s] Anonymous password : %s' ;

    // ERROR
    MSG_ERROR_PORT_VALUE : String = 'Port value invalid. Must be a integer' ;
    MSG_ERROR_MAX_CLIENT : String = 'Max client value invalid. Must be a integer' ;
    MSG_ERROR_MAX_SESSION_USER : String = 'Max session per user value invalid. Must be a integer' ;
    MSG_ERROR_PASSIVE_PORT : String = 'Invalid passive port. Use xxxx-xxxx, where xxxx is number' ;
    MSG_ERROR_START_PASSIVE_PORT : String = 'Passive port must start between 0 to 65535' ;
    MSG_ERROR_END_PASSIVE_PORT : String = 'Passive port must end between 0 to 65535' ;
    MSG_ERROR_CHECK_PASSIVE_PORT : String = 'Start passive port must be lower than end' ;
    MSG_ERROR_BUFFER_SIZE : String = 'Buffer size value invalid. Must be a integer' ;
    MSG_ERROR_USER_BYTE_RATE : String = 'Default user byte rate value invalid. Must be a integer' ;
    MSG_ERROR_ROOT_USER_NOT_FOUND : String = 'Cannot find home directory for user %s' ;
    MSG_ERROR_CANT_CREATE_SOCKET : String = 'Cannot listen port %d, maybe in use' ;

    // Ftp message
    MSG_FTP_UNAUTHORIZED : String = '421 Unauthorized.' ;
    MSG_FTP_TOO_MANY_USER : String = '421 Too many users connected.' ;
    MSG_FTP_TIME_OUT : String = '503 Time out (%d seconds).' ;
    MSG_FTP_ANONYMOUS_ALLOWED : String = '331 Anonymous access allowed, send identity (e-mail name) as password.' ;
    MSG_FTP_ANONYMOUS_NOT_ALLOWED : String = '421 Anonymous access not allowed.' ;
    MSG_FTP_SEND_PASSWORD : String = '331 Password required for %s' ;
    MSG_FTP_LOGIN_FIRST : String = '503 Login with USER first.' ;
    MSG_FTP_LOGIN_INCORRECT : String = '530 Login incorrect.' ;
    MSG_FTP_MAXIMUM_LOGIN : String = '530 Sorry, the maximum number of clients (%d) from your login are already connected.' ;
    MSG_FTP_ROOT_NOT_EXISTS : String = '530 Not logged in, cannot find home directory.' ;
    MSG_FTP_ROOT_LOGGED : String = '230 User %s logged in.' ;
    MSG_FTP_NOOP : String = '200 NOOP command successful.' ;
    MSG_FTP_CMD_NOT_UNDERSTOOD : String = '500 Command not understood.' ;
    MSG_FTP_SYST : String = '215 UNIX Type: L8' ;
    MSG_FTP_UTF8_ON : String = '200 UTF8 mode enabled' ;
    MSG_FTP_UFT8_OFF : String = '200 UTF8 mode disabled' ;
    MSG_FTP_UTF8_BAD : String = '501 Invalid UFT8 options' ;

implementation

end.

