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

#define _FILE_OFFSET_BITS 64

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <stdint.h>

#include "lzip.h"
#include "decoder.h"


const CRC32 crc32;


bool Input_buffer::read_block()
  {
  if( at_stream_end ) return false;
  stream_pos = readblock( ides_, (char *)buffer, buffer_size );
  if( stream_pos != buffer_size && errno ) throw Error( "read error" );
  pos = 0;
  at_stream_end = ( stream_pos < buffer_size );
  return !finished();
  }


void LZ_decoder::flush_data()
  {
  if( !member_finished )
    {
    const int wr = writeblock( odes_, (char *)buffer, pos );
    if( wr != pos ) throw Error( "write error" );
    if( pos >= buffer_size ) { partial_data_pos += pos; pos = 0; }
    else member_finished = true;
    }
  }


bool LZ_decoder::verify_trailer( const Pretty_print & pp ) const
  {
  bool error = false;
  File_trailer trailer;
  const int trailer_size = trailer.size( format_version );
  for( int i = 0; i < trailer_size && !error; ++i )
    {
    if( range_decoder.finished() )
      {
      error = true;
      if( verbosity >= 0 )
        {
        pp();
        std::fprintf( stderr, "trailer truncated at trailer position %d;"
                              " some checks may fail.\n", i );
        }
      }
    ((uint8_t *)&trailer)[i] = range_decoder.read_byte();
    }
  if( format_version == 0 ) trailer.member_size( member_position() );
  if( trailer.data_crc() != crc() )
    {
    error = true;
    if( verbosity >= 0 )
      {
      pp();
      std::fprintf( stderr, "crc mismatch; trailer says %08X, data crc is %08X.\n",
                    trailer.data_crc(), crc() );
      }
    }
  if( trailer.data_size() != data_position() )
    {
    error = true;
    if( verbosity >= 0 )
      {
      if( trailer.data_size() >= 0 )
        { pp();
          std::fprintf( stderr, "data size mismatch; trailer says %lld, data size is %lld.\n",
                        trailer.data_size(), data_position() ); }
      else pp( "member trailer is corrupt" );
      }
    }
  if( trailer.member_size() != member_position() )
    {
    error = true;
    if( verbosity >= 0 )
      {
      if( trailer.member_size() >= 0 )
        { pp();
          std::fprintf( stderr, "member size mismatch; trailer says %lld, member size is %lld.\n",
                        trailer.member_size(), member_position() ); }
      else pp( "member trailer is corrupt" );
      }
    }
  if( !error && verbosity >= 3 )
    std::fprintf( stderr, "data crc %08X, data size %8lld, member size %8lld.  ",
                  trailer.data_crc(), trailer.data_size(), trailer.member_size() );
  return !error;
  }


    // Return value: 0 = OK, 1 = decoder error, 2 = unexpected EOF,
    //               3 = trailer error, 4 = unknown marker found.
int LZ_decoder::decode_member( const Pretty_print & pp )
  {
  unsigned int rep0 = 0;
  unsigned int rep1 = 0;
  unsigned int rep2 = 0;
  unsigned int rep3 = 0;
  State state;
  uint8_t prev_byte = 0;

  while( true )
    {
    if( range_decoder.finished() ) return 2;
    const int pos_state = data_position() & pos_state_mask;
    if( range_decoder.decode_bit( bm_match[state()][pos_state] ) == 0 )
      {
      if( state.is_char() )
        prev_byte = literal_decoder.decode( range_decoder, prev_byte );
      else
        prev_byte = literal_decoder.decode_matched( range_decoder, prev_byte,
                                                    get_byte( rep0 ) );
      put_byte( prev_byte );
      state.set_char();
      }
    else
      {
      int len;
      if( range_decoder.decode_bit( bm_rep[state()] ) == 1 )
        {
        len = 0;
        if( range_decoder.decode_bit( bm_rep0[state()] ) == 0 )
          {
          if( range_decoder.decode_bit( bm_len[state()][pos_state] ) == 0 )
            { len = 1; state.set_short_rep(); }
          }
        else
          {
          unsigned int distance;
          if( range_decoder.decode_bit( bm_rep1[state()] ) == 0 )
            distance = rep1;
          else
            {
            if( range_decoder.decode_bit( bm_rep2[state()] ) == 0 )
              distance = rep2;
            else { distance = rep3; rep3 = rep2; }
            rep2 = rep1;
            }
          rep1 = rep0;
          rep0 = distance;
          }
        if( len == 0 )
          {
          len = min_match_len + rep_match_len_decoder.decode( range_decoder, pos_state );
          state.set_rep();
          }
        }
      else
        {
        rep3 = rep2; rep2 = rep1; rep1 = rep0;
        len = min_match_len + len_decoder.decode( range_decoder, pos_state );
        state.set_match();
        const int dis_slot = range_decoder.decode_tree( bm_dis_slot[get_dis_state(len)], dis_slot_bits );
        if( dis_slot < start_dis_model ) rep0 = dis_slot;
        else
          {
          const int direct_bits = ( dis_slot >> 1 ) - 1;
          rep0 = ( 2 | ( dis_slot & 1 ) ) << direct_bits;
          if( dis_slot < end_dis_model )
            rep0 += range_decoder.decode_tree_reversed( bm_dis + rep0 - dis_slot, direct_bits );
          else
            {
            rep0 += range_decoder.decode( direct_bits - dis_align_bits ) << dis_align_bits;
            rep0 += range_decoder.decode_tree_reversed( bm_align, dis_align_bits );
            if( rep0 == 0xFFFFFFFF )		// Marker found
              {
              if( len == min_match_len )	// End Of Stream marker
                {
                flush_data();
                if( verify_trailer( pp ) ) return 0; else return 3;
                }
              if( verbosity >= 0 )
                {
                pp();
                std::fprintf( stderr, "unsupported marker code `%d'.\n", len );
                }
              return 4;
              }
            }
          }
        }
      if( !copy_block( rep0, len ) ) return 1;
      prev_byte = get_byte( 0 );
      }
    }
  }
