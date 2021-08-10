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

{$I ftpconfig.inc}

uses
  Classes, SysUtils ;

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
    MSG_FTP_SYST : String = '215 ' ; // 215 Windows_NT
    MSG_FTP_UTF8_ON : String = '200 UTF8 mode enabled' ;
    MSG_FTP_UFT8_OFF : String = '200 UTF8 mode disabled' ;
    MSG_FTP_UTF8_BAD : String = '501 Invalid UFT8 options' ;
    MSG_FTP_FEAT_START : String = '211-Features:' ;
    MSG_FTP_FEAT_STOP : String = '211 End' ;
    MSG_FTP_PWD : String = '257 "%s" is current directory.' ;
    MSG_FTP_ACCESS_DENIED : String = '501 Permission Denied.' ;
    MSG_FTP_PASSWORD_REQUIERED : String = '503 Password requiered. Use PASS' ;
    MSG_FTP_ACCESS_DIRECTORY_DENIED : String = '550 Permission Denied.' ;
    MSG_FTP_FILE_OR_DIR_NOT_FOUND : String = '550 No such file or directory.' ;
    MSG_FTP_CWD_OK : String = '250 CWD command successful.' ;
    MSG_FTP_PORT_INVALID : String = '501 ''PORT'': Invalid number of parameters' ;
    MSG_FTP_PORT_1024 : String = '500 PORT argument must be 1024 or greater.' ;
    MSG_FTP_PORT_OK : String = '200 PORT command successful.' ;
    MSG_FTP_PASSIVE_PORT_FAIL : String = '500 PASV exception: ''No available PASV Ports''.' ;
    MSG_FTP_PASSIVE_PORT_SOCKET_FAIL : String = '500 Internal server error.' ;
    MSG_FTP_PASSIVE_PORT_OK : String = '227 Entering Passive Mode (%s).' ;
    MSG_FTP_LIST_NO_FOUND : String = '550 %s: No such file or directory.' ;
    MSG_FTP_DATA_CONNECTION_FAIL : String = '425 Can''t build data connection: Connection refused.' ;
    MSG_FTP_DATA_CONNECTION_IP_FAIL : String = '425 IP address creator connection is not the same to IP address connction.' ;
    MSG_FTP_OPEN_ASCII_DATA_CONNECTION : String = '150 Opening ASCII mode data connection.' ;
    MSG_FTP_END_DATA_CONNECTION : String = '226 Transfer complete.' ;
    MSG_FTP_TYPE_I : String = '200 Type set to I.' ;
    MSG_FTP_TYPE_A : String = '200 Type set to A.' ;
    MSG_FTP_TYPE_ERROR : String = '504 TYPE must be A or I.' ;
    MSG_FTP_NOT_A_DIRECTORY : String = '550 %s: Not a directory.' ;
implementation

end.

