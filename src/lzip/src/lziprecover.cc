/*  Lziprecover - Member recoverer program for lzip compressed files
    Copyright (C) 2008, 2009 Antonio Diaz Diaz.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
/*
    Return values: 0 for a normal exit, 1 for environmental problems
    (file not found, invalid flags, I/O errors, etc), 2 to indicate a
    corrupt or invalid input file, 3 for an internal consistency error
    (eg, bug) which caused lzip to panic.
*/

#define _FILE_OFFSET_BITS 64

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <fcntl.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/stat.h>

#include "arg_parser.h"
#include "lzip.h"


namespace {

const char * invocation_name = 0;
const char * const Program_name    = "Lziprecover";
const char * const program_name    = "lziprecover";
const char * const program_year    = "2009";


void show_help() throw()
  {
  std::printf( "%s - Member recoverer program for lzip compressed files.\n", Program_name );
  std::printf( "\nUsage: %s [options] file\n", invocation_name );
  std::printf( "Options:\n" );
  std::printf( "  -h, --help                 display this help and exit\n" );
  std::printf( "  -V, --version              output version information and exit\n" );
  std::printf( "  -q, --quiet                suppress all messages\n" );
  std::printf( "  -v, --verbose              be verbose (a 2nd -v gives more)\n" );
  std::printf( "\nReport bugs to lzip-bug@nongnu.org\n");
  }


void show_version() throw()
  {
  std::printf( "%s %s\n", Program_name, PROGVERSION );
  std::printf( "Copyright (C) %s Antonio Diaz Diaz.\n", program_year );
  std::printf( "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n" );
  std::printf( "This is free software: you are free to change and redistribute it.\n" );
  std::printf( "There is NO WARRANTY, to the extent permitted by law.\n" );
  }


int open_instream( const std::string & input_filename ) throw()
  {
  int ides = open( input_filename.c_str(), O_RDONLY );
  if( ides < 0 )
    {
    if( verbosity >= 0 )
      std::fprintf( stderr, "%s: Can't open input file `%s': %s.\n",
                    program_name, input_filename.c_str(), std::strerror( errno ) );
    }
  else
    {
    struct stat in_stats;
    const int i = fstat( ides, &in_stats );
    if( i < 0 || !S_ISREG( in_stats.st_mode ) )
      {
      if( verbosity >= 0 )
        std::fprintf( stderr, "%s: input file `%s' is not a regular file.\n",
                      program_name, input_filename.c_str() );
      close( ides );
      ides = -1;
      }
    }
  return ides;
  }


int open_outstream( const std::string & output_filename ) throw()
  {
  int odes = open( output_filename.c_str(), O_CREAT | O_TRUNC | O_WRONLY, S_IRUSR | S_IWUSR );
  if( odes < 0 )
    {
    if( verbosity >= 0 )
      std::fprintf( stderr, "%s: Can't create output file `%s': %s.\n",
                    program_name, output_filename.c_str(), std::strerror( errno ) );
    }
  return odes;
  }


bool next_filename( std::string & output_filename )
  {
  for( int i = 7; i >= 3; --i )			// "rec00001"
    {
    if( output_filename[i] < '9' ) { ++output_filename[i]; return true; }
    else output_filename[i] = '0';
    }
  return false;
  }


int search_header( const char * buffer, const int size, const int pos,
                   const long long last_header_pos,
                   const long long partial_file_pos )
  {
  for( int i = pos; i < size; ++i )
    if( buffer[i] == magic_string[0] && buffer[i+1] == magic_string[1] &&
        buffer[i+2] == magic_string[2] && buffer[i+3] == magic_string[3] )
      {
      File_trailer trailer;
      for( unsigned int j = 0; j < sizeof trailer; ++j )
        ((char *)&trailer)[j] = buffer[i-(sizeof trailer)+j];
      if( partial_file_pos + i - trailer.member_size() == last_header_pos )
        return i;
      }
  return -1;
  }


bool verify_header( const char * buffer, const int pos )
  {
  File_header header;
  for( unsigned int i = 0; i < sizeof header; ++i )
    ((char *)&header)[i] = buffer[pos+i];
  if( !header.verify_magic() )
    {
    show_error( "bad magic number (file not created by lzip).\n" );
    return false;
    }
  if( header.version == 0 )
    {
    show_error( "version 0 member format can't be recovered.\n" );
    return false;
    }
  if( header.version != 1 )
    {
    if( verbosity >= 0 )
      std::fprintf( stderr, "version %d member format not supported, newer %s needed.\n",
                    header.version, program_name );
    return false;
    }
  return true;
  }


int process_file( const std::string & input_filename, char * & base_buffer )
  {
  const int hsize = sizeof( File_header );
  const int tsize = sizeof( File_trailer );
  const int buffer_size = 65536;
  const int base_buffer_size = tsize + buffer_size + hsize;
  base_buffer = new char[base_buffer_size];
  char * const buffer = base_buffer + tsize;

  const int inhandle = open_instream( input_filename );
  if( inhandle < 0 ) return 1;
  int size = readblock( inhandle, buffer, buffer_size + hsize ) - hsize;
  bool at_stream_end = ( size < buffer_size );
  if( size != buffer_size && errno )
    { show_error( "read error", errno ); return 1; }
  if( size <= tsize )
    { show_error( "file too short" ); return 2; }
  if( !verify_header( buffer, 0 ) ) return 2;

  std::string output_filename( "rec00001" ); output_filename += input_filename;
  int outhandle = open_outstream( output_filename );
  if( outhandle < 0 ) { close( inhandle ); return 1; }

  long long last_header_pos = 0;
  long long partial_file_pos = 0;
  int pos = 0;
  while( size > 0 )
    {
    const int newpos = search_header( buffer, size - hsize, pos + hsize,
                                      last_header_pos, partial_file_pos );
    if( newpos > pos )
      {
      const int wr = writeblock( outhandle, buffer + pos, newpos - pos );
      if( wr != newpos - pos )
        { show_error( "write error", errno ); return 1; }
      if( close( outhandle ) != 0 )
        { show_error( "error closing output file", errno ); return 1; }
      if( !next_filename( output_filename ) )
        { show_error( "too many members in file" ); close( inhandle ); return 1; }
      outhandle = open_outstream( output_filename );
      if( outhandle < 0 ) { close( inhandle ); return 1; }
      last_header_pos = partial_file_pos + newpos;
      pos = newpos;
      continue;
      }
    else
      {
      if( !at_stream_end )
        {
        partial_file_pos += buffer_size;
        const int wr = writeblock( outhandle, buffer + pos, buffer_size - pos );
        if( wr != buffer_size - pos )
          { show_error( "write error", errno ); return 1; }
        std::memcpy( base_buffer, base_buffer + buffer_size, tsize + hsize );
        pos = 0;
        }
      else
        {
        const int wr = writeblock( outhandle, buffer + pos, size + hsize - pos );
        if( wr != size + hsize - pos )
          { show_error( "write error", errno ); return 1; }
        break;
        }
      }
    size = readblock( inhandle, buffer + hsize, buffer_size );
    at_stream_end = ( size < buffer_size );
    if( size != buffer_size && errno )
      { show_error( "read error", errno ); return 1; }
    }
  close( inhandle );
  if( close( outhandle ) != 0 )
    { show_error( "error closing output file", errno ); return 1; }
  return 0;
  }

} // end namespace


int verbosity = 0;


void show_error( const char * msg, const int errcode, const bool help ) throw()
  {
  if( verbosity >= 0 )
    {
    if( msg && msg[0] != 0 )
      {
      std::fprintf( stderr, "%s: %s", program_name, msg );
      if( errcode > 0 ) std::fprintf( stderr, ": %s", std::strerror( errcode ) );
      std::fprintf( stderr, "\n" );
      }
    if( help && invocation_name && invocation_name[0] != 0 )
      std::fprintf( stderr, "Try `%s --help' for more information.\n", invocation_name );
    }
  }


void internal_error( const char * msg )
  {
  std::string s( "internal error: " ); s += msg;
  show_error( s.c_str() );
  std::exit( 3 );
  }


// Returns the number of bytes really read.
// If (returned value < size) and (errno == 0), means EOF was reached.
//
int readblock( const int fd, char * buf, const int size ) throw()
  {
  int rest = size;
  errno = 0;
  while( rest > 0 )
    {
    errno = 0;
    const int n = read( fd, buf + size - rest, rest );
    if( n > 0 ) rest -= n;
    else if( n == 0 ) break;
    else if( errno != EINTR && errno != EAGAIN ) break;
    }
  return ( rest > 0 ) ? size - rest : size;
  }


// Returns the number of bytes really written.
// If (returned value < size), it is always an error.
//
int writeblock( const int fd, const char * buf, const int size ) throw()
  {
  int rest = size;
  errno = 0;
  while( rest > 0 )
    {
    errno = 0;
    const int n = write( fd, buf + size - rest, rest );
    if( n > 0 ) rest -= n;
    else if( errno && errno != EINTR && errno != EAGAIN ) break;
    }
  return ( rest > 0 ) ? size - rest : size;
  }


int main( const int argc, const char * argv[] )
  {
  invocation_name = argv[0];

  const Arg_parser::Option options[] =
    {
    { 'h', "help",            Arg_parser::no  },
    { 'q', "quiet",           Arg_parser::no  },
    { 'v', "verbose",         Arg_parser::no  },
    { 'V', "version",         Arg_parser::no  },
    {  0 ,  0,                Arg_parser::no  } };

  Arg_parser parser( argc, argv, options );
  if( parser.error().size() )				// bad option
    { show_error( parser.error().c_str(), 0, true ); return 1; }

  int argind = 0;
  for( ; argind < parser.arguments(); ++argind )
    {
    const int code = parser.code( argind );
    if( !code ) break;					// no more options
    switch( code )
      {
      case 'h': show_help(); return 0;
      case 'q': verbosity = -1; break;
      case 'v': if( verbosity < 4 ) ++verbosity; break;
      case 'V': show_version(); return 0;
      default : internal_error( "uncaught option" );
      }
    }

  if( argind + 1 != parser.arguments() )
    { show_error( "you must specify exactly 1 file", 0, true ); return 1; }

  char * base_buffer;
  const int retval = process_file( parser.argument( argind ), base_buffer );

  delete[] base_buffer;
  return retval;
  }
