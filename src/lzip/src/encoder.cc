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

#include <algorithm>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <stdint.h>

#include "lzip.h"
#include "encoder.h"


Dis_slots dis_slots;
Prob_prices prob_prices;


bool Matchfinder::read_block() throw()
  {
  const int size = buffer_size - stream_pos;
  const int rd = readblock( ides_, (char *)buffer + stream_pos, size );
  stream_pos += rd;
  if( rd < size ) at_stream_end = true;
  return ( rd == size || !errno );
  }


bool Matchfinder::reset() throw()
  {
  const int size = stream_pos - pos;
  std::memmove( buffer, buffer + pos, size );
  partial_data_pos = 0;
  stream_pos -= pos;
  pos = 0;
  cyclic_pos = 0;
  for( int i = 0; i < num_prev_positions; ++i ) prev_positions[i] = -1;
  return ( at_stream_end || read_block() );
  }


bool Matchfinder::move_pos() throw()
  {
  if( ++cyclic_pos >= dictionary_size_ ) cyclic_pos = 0;
  if( ++pos >= pos_limit )
    {
    if( !at_stream_end )
      {
      const int offset = pos - dictionary_size_ - max_num_trials;
      const int size = stream_pos - offset;
      std::memmove( buffer, buffer + offset, size );
      partial_data_pos += offset;
      pos -= offset;
      stream_pos -= offset;
      for( int i = 0; i < num_prev_positions; ++i )
        if( prev_positions[i] >= 0 ) prev_positions[i] -= offset;
      for( int i = 0; i < 2 * dictionary_size_; ++i )
        if( prev_pos_tree[i] >= 0 ) prev_pos_tree[i] -= offset;
      return read_block();
      }
    else if( pos > stream_pos ) { pos = stream_pos; return false; }
    }
  return true;
  }


int Matchfinder::longest_match_len( int * const distances ) throw()
  {
  int len_limit = match_len_limit_;
  if( len_limit > available_bytes() )
    {
    len_limit = available_bytes();
    if( len_limit < 4 ) return 0;
    }

  int maxlen = min_match_len - 1;
  const int min_pos = (pos >= dictionary_size_) ?
                      (pos - dictionary_size_ + 1) : 0;
  const uint8_t * const data = buffer + pos;
  const int key2 = num_prev_positions4 + num_prev_positions3 +
                   ( ( (int)data[0] << 8 ) | data[1] );
  const int tmp = crc32[data[0]] ^ data[1] ^ ( (int)data[2] << 8 );
  const int key3 = num_prev_positions4 + ( tmp & ( num_prev_positions3 - 1 ) );
  const int key4 = ( tmp ^ ( crc32[data[3]] << 5 ) ) &
                   ( num_prev_positions4 - 1 );

  if( distances )
    {
    int np = prev_positions[key2];
    if( np >= min_pos )
      { distances[2] = pos - np - 1; maxlen = 2; }
    else distances[2] = 0x7FFFFFFF;
    np = prev_positions[key3];
    if( np >= min_pos && buffer[np] == data[0] )
      { distances[3] = pos - np - 1; maxlen = 3; }
    else distances[3] = 0x7FFFFFFF;
    distances[4] = 0x7FFFFFFF;
    }

  prev_positions[key2] = pos;
  prev_positions[key3] = pos;
  int newpos = prev_positions[key4];
  prev_positions[key4] = pos;

  int idx0 = cyclic_pos << 1;
  int idx1 = idx0 + 1;
  int len0 = 0, len1 = 0;

  for( int count = 16 + ( match_len_limit_ / 2 ); ; )
    {
    if( newpos < min_pos || --count < 0 )
      { prev_pos_tree[idx0] = prev_pos_tree[idx1] = -1; break; }
    const uint8_t * const newdata = buffer + newpos;
    int len = std::min( len0, len1 );
    while( len < len_limit && newdata[len] == data[len] ) ++len;

    const int delta = pos - newpos;
    if( distances ) while( maxlen < len ) distances[++maxlen] = delta - 1;

    const int newidx = ( cyclic_pos - delta +
      ( ( cyclic_pos >= delta ) ? 0 : dictionary_size_ ) ) << 1;

    if( len < len_limit )
      {
      if( newdata[len] < data[len] )
        {
        prev_pos_tree[idx0] = newpos;
        idx0 = newidx + 1;
        newpos = prev_pos_tree[idx0];
        len0 = len;
        }
      else
        {
        prev_pos_tree[idx1] = newpos;
        idx1 = newidx;
        newpos = prev_pos_tree[idx1];
        len1 = len;
        }
      }
    else
      {
      prev_pos_tree[idx0] = prev_pos_tree[newidx];
      prev_pos_tree[idx1] = prev_pos_tree[newidx+1];
      break;
      }
    }
  if( distances )
    {
    if( distances[3] > distances[4] ) distances[3] = distances[4];
    if( distances[2] > distances[3] ) distances[2] = distances[3];
    }
  return maxlen;
  }


void Len_encoder::encode( Range_encoder & range_encoder, int symbol,
                          const int pos_state )
  {
  symbol -= min_match_len;
  if( symbol < len_low_symbols )
    {
    range_encoder.encode_bit( choice1, 0 );
    range_encoder.encode_tree( bm_low[pos_state], symbol, len_low_bits );
    }
  else
    {
    range_encoder.encode_bit( choice1, 1 );
    if( symbol < len_low_symbols + len_mid_symbols )
      {
      range_encoder.encode_bit( choice2, 0 );
      range_encoder.encode_tree( bm_mid[pos_state], symbol - len_low_symbols, len_mid_bits );
      }
    else
      {
      range_encoder.encode_bit( choice2, 1 );
      range_encoder.encode_tree( bm_high, symbol - len_low_symbols - len_mid_symbols, len_high_bits );
      }
    }
  if( --counters[pos_state] <= 0 ) update_prices( pos_state );
  }


void LZ_encoder::fill_align_prices() throw()
  {
  for( int i = 0; i < dis_align_size; ++i )
    align_prices[i] = price_symbol_reversed( bm_align, i, dis_align_bits );
  align_price_count = dis_align_size;
  }


void LZ_encoder::fill_distance_prices() throw()
  {
  for( int dis_state = 0; dis_state < max_dis_states; ++dis_state )
    {
    int * dsp = dis_slot_prices[dis_state];
    const Bit_model * bmds = bm_dis_slot[dis_state];
    int slot = 0;
    for( ; slot < end_dis_model && slot < num_dis_slots; ++slot )
      dsp[slot] = price_symbol( bmds, slot, dis_slot_bits );
    for( ; slot < num_dis_slots; ++slot )
      dsp[slot] = price_symbol( bmds, slot, dis_slot_bits ) +
                  (((( slot >> 1 ) - 1 ) - dis_align_bits ) << price_shift );

    int * dp = dis_prices[dis_state];
    int dis = 0;
    for( ; dis < start_dis_model; ++dis )
      dp[dis] = dsp[dis];
    for( ; dis < modeled_distances; ++dis )
      {
      const int dis_slot = dis_slots[dis];
      const int direct_bits = ( dis_slot >> 1 ) - 1;
      const int base = ( 2 | ( dis_slot & 1 ) ) << direct_bits;
      dp[dis] = dsp[dis_slot] +
          price_symbol_reversed( bm_dis + base - dis_slot, dis - base, direct_bits );
      }
    }
  }


// Return value: ( dis == -1 ) && ( len == 1 ) means literal
int LZ_encoder::best_pair_sequence( const int reps[num_rep_distances],
                                    const State & state )
  {
  int main_len;
  if( longest_match_found > 0 )		// from previous call
    {
    main_len = longest_match_found;
    longest_match_found = 0;
    }
  else main_len = read_match_distances();

  int replens[num_rep_distances];
  int rep_index = 0;
  for( int i = 0; i < num_rep_distances; ++i )
    {
    replens[i] = matchfinder.true_match_len( 0, reps[i] + 1, max_match_len );
    if( replens[i] > replens[rep_index] ) rep_index = i;
    }
  if( replens[rep_index] >= matchfinder.match_len_limit() )
    {
    trials[0].dis = rep_index;
    trials[0].price = replens[rep_index];
    if( !move_pos( replens[rep_index], true ) ) return 0;
    return replens[rep_index];
    }

  if( main_len >= matchfinder.match_len_limit() )
    {
    trials[0].dis = match_distances[matchfinder.match_len_limit()] +
                    num_rep_distances;
    trials[0].price = main_len;
    if( !move_pos( main_len, true ) ) return 0;
    return main_len;
    }

  trials[0].state = state;
  for( int i = 0; i < num_rep_distances; ++i ) trials[0].reps[i] = reps[i];

  const uint8_t cur_byte = matchfinder[0];
  const uint8_t match_byte = matchfinder[-reps[0]-1];
  unsigned int position = matchfinder.data_position();
  const int pos_state = position & pos_state_mask;

  trials[1].dis = -1;
  trials[1].prev_index = 0;
  trials[1].price = price0( bm_match[state()][pos_state] );
  if( state.is_char() )
    trials[1].price += literal_encoder.price_symbol( matchfinder[-1], cur_byte );
  else
    trials[1].price += literal_encoder.price_matched( matchfinder[-1], cur_byte, match_byte );

  const int match_price = price1( bm_match[state()][pos_state] );
  const int rep_match_price = match_price + price1( bm_rep[state()] );

  if( match_byte == cur_byte )
    trials[1].update( 0, 0, rep_match_price + price_rep_len1( state, pos_state ) );

  if( main_len < min_match_len )
    {
    trials[0].dis = trials[1].dis;
    trials[0].price = 1;
    if( !matchfinder.move_pos() ) return 0;
    return 1;
    }

  {
  const int normal_match_price = match_price + price0( bm_rep[state()] );
  int len = min_match_len;
  if( main_len <= replens[rep_index] )
    {
    main_len = replens[rep_index];
    for( ; len <= main_len; ++len ) trials[len].price = infinite_price;
    }
  else for( ; len <= main_len; ++len )
    {
    trials[len].dis = match_distances[len] + num_rep_distances;
    trials[len].prev_index = 0;
    trials[len].price = normal_match_price +
                        price_pair( match_distances[len], len, pos_state );
    }
  }

  for( int rep = 0; rep < num_rep_distances; ++rep )
    for( int len = min_match_len; len <= replens[rep]; ++len )
      trials[len].update( rep, 0, rep_match_price +
                                  price_rep( rep, len, state, pos_state ) );

  int cur = 0;
  int num_trials = main_len;
  if( !matchfinder.move_pos() ) return 0;

  while( true )
    {
    if( ++cur >= num_trials )
      {
      backward( cur );
      return cur;
      }
    const int newlen = read_match_distances();
    if( newlen >= matchfinder.match_len_limit() )
      {
      longest_match_found = newlen;
      backward( cur );
      return cur;
      }

    Trial & cur_trial = trials[cur];
    const int prev_index = cur_trial.prev_index;

    cur_trial.state = trials[prev_index].state;

    for( int i = 0; i < num_rep_distances; ++i )
      cur_trial.reps[i] = trials[prev_index].reps[i];
    if( prev_index == cur - 1 )
      {
      if( cur_trial.dis == 0 ) cur_trial.state.set_short_rep();
      else cur_trial.state.set_char();
      }
    else
      {
      if( cur_trial.dis < num_rep_distances ) cur_trial.state.set_rep();
      else cur_trial.state.set_match();
      mtf_reps( cur_trial.dis, cur_trial.reps );
      }

    const uint8_t cur_byte = matchfinder[0];
    const uint8_t match_byte = matchfinder[-cur_trial.reps[0]-1];
    const int pos_state = ++position & pos_state_mask;
    int next_price = cur_trial.price + price0( bm_match[cur_trial.state()][pos_state] );
    if( cur_trial.state.is_char() )
      next_price += literal_encoder.price_symbol( matchfinder[-1], cur_byte );
    else
      next_price += literal_encoder.price_matched( matchfinder[-1], cur_byte, match_byte );
    if( !matchfinder.move_pos() ) return 0;

    Trial & next_trial = trials[cur+1];

    next_trial.update( -1, cur, next_price );

    const int match_price = cur_trial.price + price1( bm_match[cur_trial.state()][pos_state] );
    const int rep_match_price = match_price + price1( bm_rep[cur_trial.state()] );

    if( match_byte == cur_byte && next_trial.dis != 0 )
      next_trial.update( 0, cur, rep_match_price +
                                 price_rep_len1( cur_trial.state, pos_state ) );

    const int len_limit = std::min( std::min( max_num_trials - 1 - cur,
                          matchfinder.available_bytes() ), matchfinder.match_len_limit() );
    if( len_limit < min_match_len ) continue;

    for( int rep = 0; rep < num_rep_distances; ++rep )
      {
      const int dis = cur_trial.reps[rep] + 1;
      int len = 0;
      const uint8_t * const data = matchfinder.ptr_to_current_pos() - 1;
      while( len < len_limit && data[len] == data[len-dis] ) ++len;
      if( len >= min_match_len )
        {
        while( num_trials < cur + len )
          trials[++num_trials].price = infinite_price;
        for( ; len >= min_match_len; --len )
          trials[cur+len].update( rep, cur, rep_match_price +
                                  price_rep( rep, len, cur_trial.state, pos_state ) );
        }
      }

    if( newlen <= len_limit &&
        ( newlen > min_match_len ||
          ( newlen == min_match_len &&
            match_distances[newlen] < modeled_distances ) ) )
      {
      const int normal_match_price = match_price +
                                     price0( bm_rep[cur_trial.state()] );
      while( num_trials < cur + newlen )
        trials[++num_trials].price = infinite_price;

      for( int len = newlen; len >= min_match_len; --len )
        trials[cur+len].update( match_distances[len] + num_rep_distances, cur,
                                normal_match_price +
                                price_pair( match_distances[len], len, pos_state ) );
      }
    }
  }


     // End Of Stream mark => (dis == 0xFFFFFFFF, len == min_match_len)
void LZ_encoder::flush( const State & state )
  {
  const int pos_state = ( matchfinder.data_position() ) & pos_state_mask;
  range_encoder.encode_bit( bm_match[state()][pos_state], 1 );
  range_encoder.encode_bit( bm_rep[state()], 0 );
  encode_pair( 0xFFFFFFFF, min_match_len, pos_state );
  range_encoder.flush();
  File_trailer trailer;
  trailer.data_crc( crc() );
  trailer.data_size( matchfinder.data_position() );
  trailer.member_size( range_encoder.member_position() + sizeof trailer );
  for( unsigned int i = 0; i < sizeof trailer; ++i )
    range_encoder.put_byte( (( uint8_t *)&trailer)[i] );
  range_encoder.flush_data();
  }


LZ_encoder::LZ_encoder( Matchfinder & mf, const File_header & header,
                        const int odes )
  :
  longest_match_found( 0 ),
  crc_( 0xFFFFFFFF ),
  matchfinder( mf ),
  range_encoder( odes ),
  len_encoder( matchfinder.match_len_limit() ),
  rep_match_len_encoder( matchfinder.match_len_limit() ),
  literal_encoder(),
  num_dis_slots( 2 * File_header::real_bits( matchfinder.dictionary_size() - 1 ) )
  {
  fill_align_prices();

  for( unsigned int i = 0; i < sizeof header; ++i )
    range_encoder.put_byte( (( uint8_t *)&header)[i] );
  }


bool LZ_encoder::encode_member( const long long member_size )
  {
  if( range_encoder.member_position() != sizeof( File_header ) )
    return false;		// can be called only once
  const long long member_size_limit = member_size - sizeof( File_trailer ) - 15;
  int fill_counter = 0;
  int rep_distances[num_rep_distances];
  State state;
  uint8_t prev_byte = 0;
  for( int i = 0; i < num_rep_distances; ++i ) rep_distances[i] = 0;

  if( !matchfinder.finished() )			// copy first byte
    {
    range_encoder.encode_bit( bm_match[state()][0], 0 );
    const uint8_t cur_byte = matchfinder[0];
    literal_encoder.encode( range_encoder, prev_byte, cur_byte );
    prev_byte = cur_byte;
    crc32.update( crc_, cur_byte );
    if( !move_pos( 1 ) ) return false;
    }

  while( true )
    {
    if( matchfinder.finished() ) { flush( state ); return true; }
    if( fill_counter <= 0 ) { fill_distance_prices(); fill_counter = 512; }

    int ahead = best_pair_sequence( rep_distances, state );
    if( ahead <= 0 ) return false;
    fill_counter -= ahead;

    for( int i = 0; ; )
      {
      const int pos_state = ( matchfinder.data_position() - ahead ) & pos_state_mask;
      int dis = trials[i].dis;
      const int len = trials[i].price;

      bool bit = ( dis < 0 && len == 1 );
      range_encoder.encode_bit( bm_match[state()][pos_state], !bit );
      if( bit )
        {
        const uint8_t cur_byte = matchfinder[-ahead];
        if( state.is_char() )
          literal_encoder.encode( range_encoder, prev_byte, cur_byte );
        else
          {
          const uint8_t match_byte = matchfinder[-rep_distances[0]-1-ahead];
          literal_encoder.encode_matched( range_encoder, prev_byte, match_byte, cur_byte );
          }
        state.set_char();
        prev_byte = cur_byte;
        }
      else
        {
        mtf_reps( dis, rep_distances );
        bit = ( dis < num_rep_distances );
        range_encoder.encode_bit( bm_rep[state()], bit );
        if( bit )
          {
          bit = ( dis == 0 );
          range_encoder.encode_bit( bm_rep0[state()], !bit );
          if( bit )
            range_encoder.encode_bit( bm_len[state()][pos_state], len > 1 );
          else
            {
            range_encoder.encode_bit( bm_rep1[state()], dis > 1 );
            if( dis > 1 )
              range_encoder.encode_bit( bm_rep2[state()], dis > 2 );
            }
          if( len == 1 ) state.set_short_rep();
          else
            {
            rep_match_len_encoder.encode( range_encoder, len, pos_state );
            state.set_rep();
            }
          }
        else
          {
          encode_pair( dis - num_rep_distances, len, pos_state );
          state.set_match();
          }
        prev_byte = matchfinder[len-1-ahead];
        }
      for( int j = 0; j < len; ++j )
        crc32.update( crc_, matchfinder[j-ahead] );
      ahead -= len; i += len;
      if( range_encoder.member_position() >= member_size_limit )
        {
        if( !matchfinder.dec_pos( ahead ) ) return false;
        flush( state );
        return true;
        }
      if( ahead <= 0 ) break;
      }
    }
  }
