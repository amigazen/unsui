/*  Lzip - A data compressor based on the LZMA algorithm
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

class State
  {
  unsigned char st;

public:
  enum { states = 12 };
  State() throw() : st( 0 ) {}
  int operator()() const throw() { return st; }
  bool is_char() const throw() { return st < 7; }

  void set_char() throw()
    {
    static const unsigned char next[states] = {0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 4, 5};
    st = next[st];
    }
  void set_match() throw()
    {
    static const unsigned char next[states] = {7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10};
    st = next[st];
    }
  void set_rep() throw()
    {
    static const unsigned char next[states] = {8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11};
    st = next[st];
    }
  void set_short_rep() throw()
    {
    static const unsigned char next[states] = {9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11};
    st = next[st];
    }
  };


const int min_dictionary_bits = 12;
const int min_dictionary_size = 1 << min_dictionary_bits;
const int max_dictionary_bits = 29;
const int max_dictionary_size = 1 << max_dictionary_bits;
const int literal_context_bits = 3;
const int pos_state_bits = 2;
const int pos_states = 1 << pos_state_bits;
const int pos_state_mask = pos_states - 1;

const int dis_slot_bits = 6;
const int start_dis_model = 4;
const int end_dis_model = 14;
const int modeled_distances = 1 << (end_dis_model / 2);
const int dis_align_bits = 4;
const int dis_align_size = 1 << dis_align_bits;

const int len_low_bits = 3;
const int len_mid_bits = 3;
const int len_high_bits = 8;
const int len_low_symbols = 1 << len_low_bits;
const int len_mid_symbols = 1 << len_mid_bits;
const int len_high_symbols = 1 << len_high_bits;
const int max_len_symbols = len_low_symbols + len_mid_symbols + len_high_symbols;

const int min_match_len = 2;		// must be 2
const int max_match_len = min_match_len + max_len_symbols - 1;	// 273

const int max_dis_states = 4;

inline int get_dis_state( int len ) throw()
  {
  len -= min_match_len;
  if( len >= max_dis_states ) len = max_dis_states - 1;
  return len;
  }


const int bit_model_move_bits = 5;
const int bit_model_total_bits = 11;
const int bit_model_total = 1 << bit_model_total_bits;

struct Bit_model
  {
  unsigned int probability;
  Bit_model() throw() : probability( bit_model_total / 2 ) {}
  };


class Pretty_print
  {
  const char * const stdin_name;
  const unsigned int stdin_name_len;
  unsigned int longest_name;
  std::string name_;
  mutable bool first_post;

public:
  Pretty_print( const std::vector< std::string > & filenames )
    : stdin_name( "(stdin)" ), stdin_name_len( std::strlen( stdin_name ) ),
      longest_name( 0 ), first_post( false )
    {
    for( unsigned int i = 0; i < filenames.size(); ++i )
      {
      const std::string & s = filenames[i];
      const unsigned int len = ( ( s == "-" ) ? stdin_name_len : s.size() );
      if( len > longest_name ) longest_name = len;
      }
    if( longest_name == 0 ) longest_name = stdin_name_len;
    }

  void set_name( const std::string & filename )
    {
    if( filename.size() && filename != "-" ) name_ = filename;
    else name_ = stdin_name;
    first_post = true;
    }

  void reset() const throw() { if( name_.size() ) first_post = true; }
  const char * name() const throw() { return name_.c_str(); }
  void operator()( const char * const msg = 0 ) const throw();
  };


class CRC32
  {
  uint32_t data[256];		// Table of CRCs of all 8-bit messages.

public:
  CRC32()
    {
    for( unsigned int n = 0; n < 256; ++n )
      {
      unsigned int c = n;
      for( int k = 0; k < 8; ++k )
        { if( c & 1 ) c = 0xEDB88320 ^ ( c >> 1 ); else c >>= 1; }
      data[n] = c;
      }
    }

  uint32_t operator[]( const uint8_t byte ) const throw() { return data[byte]; }
  void update( uint32_t & crc, const uint8_t byte ) const throw()
    { crc = data[(crc^byte)&0xFF] ^ ( crc >> 8 ); }
  };


const char * const magic_string = "LZIP";

struct File_header
  {
  char magic[4];
  uint8_t version;
  uint8_t coded_dict_size;

  void set_magic() throw()
    { std::memcpy( magic, magic_string, sizeof magic ); version = 1; }

  bool verify_magic() const throw()
    {
    return ( std::memcmp( magic, magic_string, sizeof magic ) == 0 );
    }

  bool verify_version() const throw()
    {
    return ( version <= 1 );
    }

  static int real_bits( const int value ) throw()
    {
    int bits = 0;
    for( int i = 1, mask = 1; mask > 0; ++i, mask <<= 1 )
      if( value & mask ) bits = i;
    return bits;
    }

  int dictionary_size() const throw()
    {
    int size = ( 1 << ( coded_dict_size & 0x1F ) );
    if( size > min_dictionary_size && size <= max_dictionary_size )
      size -= ( size / 16 ) * ( ( coded_dict_size >> 5 ) & 0x07 );
    return size;
    }

  bool dictionary_size( const int size ) throw()
    {
    if( size >= min_dictionary_size && size <= max_dictionary_size )
      {
      coded_dict_size = real_bits( size - 1 );
      if( size > min_dictionary_size )
        {
        const int base_size = 1 << coded_dict_size;
        const int wedge = base_size / 16;
        for( int i = 7; i >= 1; --i )
          if( base_size - ( i * wedge ) >= size )
            { coded_dict_size |= ( i << 5 ); break; }
        }
      return true;
      }
    return false;
    }
  };


struct File_trailer
  {
  uint8_t data_crc_[4];		// CRC32 of the uncompressed data
  uint8_t data_size_[8];	// size of the uncompressed data
  uint8_t member_size_[8];	// member size including header and trailer

  static int size( const int version )
    { return sizeof( File_trailer ) - ( ( version >= 1 ) ? 0 : 8 ); }

  uint32_t data_crc() const throw()
    {
    uint32_t tmp = 0;
    for( int i = 3; i >= 0; --i ) { tmp <<= 8; tmp += data_crc_[i]; }
    return tmp;
    }

  void data_crc( uint32_t crc ) throw()
    {
    for( int i = 0; i < 4; ++i )
      { data_crc_[i] = (uint8_t)crc; crc >>= 8; }
    }

  long long data_size() const throw()
    {
    long long tmp = 0;
    for( int i = 7; i >= 0; --i ) { tmp <<= 8; tmp += data_size_[i]; }
    return tmp;
    }

  void data_size( long long size ) throw()
    {
    for( int i = 0; i < 8; ++i )
      { data_size_[i] = (uint8_t)size; size >>= 8; }
    }

  long long member_size() const throw()
    {
    long long tmp = 0;
    for( int i = 7; i >= 0; --i ) { tmp <<= 8; tmp += member_size_[i]; }
    return tmp;
    }

  void member_size( long long size ) throw()
    {
    for( int i = 0; i < 8; ++i )
      { member_size_[i] = (uint8_t)size; size >>= 8; }
    }
  };


struct Error
  {
  const char * s;
  Error( const char * p ) throw() : s( p ) {}
  };

extern int verbosity;
extern const CRC32 crc32;

void show_error( const char * msg, const int errcode = 0, const bool help = false ) throw();
void internal_error( const char * msg );
int readblock( const int fd, char * buf, const int size ) throw();
int writeblock( const int fd, const char * buf, const int size ) throw();
