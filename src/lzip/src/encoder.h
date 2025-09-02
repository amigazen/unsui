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

const int max_num_trials = 1 << 12;
const int price_shift = 6;

class Dis_slots
  {
  unsigned char data[1<<12];

public:
  void init() throw()
    {
    for( int slot = 0; slot < 4; ++slot ) data[slot] = slot;
    for( int i = 4, size = 2, slot = 4; slot < 24; slot += 2 )
      {
      std::memset( &data[i], slot, size );
      std::memset( &data[i+size], slot + 1, size );
      size <<= 1;
      i += size;
      }
    }

  int operator[]( const uint32_t dis ) const throw()
    {
    if( dis < (1 << 12) ) return data[dis];
    if( dis < (1 << 23) ) return data[dis>>11] + 22;
    return data[dis>>22] + 44;
    }
  };

extern Dis_slots dis_slots;

class Prob_prices
  {
  int data[bit_model_total >> 2];

public:
  void init() throw()
    {
    const int num_bits = ( bit_model_total_bits - 2 );
    for( int i = num_bits - 1; i >= 0; --i )
      {
      int start = 1 << ( num_bits - i - 1 );
      int end = 1 << ( num_bits - i);
      for( int j = start; j < end; ++j )
        data[j] = (i << price_shift) +
            ( ((end - j) << price_shift) >> (num_bits - i - 1) );
      }
    }

  int operator[]( const int symbol ) const throw()
    { return data[symbol >> 2]; }
  };

extern Prob_prices prob_prices;

inline int price0( const Bit_model & bm ) throw()
  { return prob_prices[bm.probability]; }

inline int price1( const Bit_model & bm ) throw()
  { return prob_prices[bit_model_total-bm.probability]; }

inline int price_bit( const Bit_model & bm, const int bit ) throw()
  { if( bit ) return price1( bm ); else return price0( bm ); }

inline int price_symbol( const Bit_model bm[], int symbol, const int num_bits ) throw()
  {
  symbol |= ( 1 << num_bits );
  int price = 0;
  while( symbol > 1 )
    {
    const int bit = symbol & 1;
    symbol >>= 1;
    price += price_bit( bm[symbol-1], bit );
    }
  return price;
  }

inline int price_symbol_reversed( const Bit_model bm[], int symbol,
                                  const int num_bits ) throw()
  {
  int price = 0;
  int model = 1;
  for( int i = num_bits; i > 0; --i )
    {
    const int bit = symbol & 1;
    symbol >>= 1;
    price += price_bit( bm[model-1], bit );
    model = ( model << 1 ) | bit;
    }
  return price;
  }

inline int price_matched( const Bit_model bm[], const int symbol,
                          const int match_byte ) throw()
  {
  int price = 0;
  int model = 1;

  for( int i = 7; i >= 0; --i )
    {
    const int match_bit = ( match_byte >> i ) & 1;
    const int bit = ( symbol >> i ) & 1;
    price += price_bit( bm[(match_bit<<8)+model+0xFF], bit );
    model = ( model << 1 ) | bit;
    if( match_bit != bit )
      {
      while( --i >= 0 )
        {
        const int bit = ( symbol >> i ) & 1;
        price += price_bit( bm[model-1], bit );
        model = ( model << 1 ) | bit;
        }
      break;
      }
    }
  return price;
  }


class Matchfinder
  {
  enum { num_prev_positions4 = 1 << 20,
         num_prev_positions3 = 1 << 18,
         num_prev_positions2 = 1 << 16,
         num_prev_positions = num_prev_positions4 + num_prev_positions3 +
                              num_prev_positions2 };

  long long partial_data_pos;
  int dictionary_size_;		// bytes to keep in buffer before pos
  const int after_size;		// bytes to keep in buffer after pos
  const int buffer_size;
  uint8_t * const buffer;
  int pos;
  int cyclic_pos;
  int stream_pos;		// first byte not yet read from file
  const int pos_limit;		// when reached, a new block must be read
  const int ides_;
  const int match_len_limit_;
  int32_t * const prev_positions;	// last seen position of key
  int32_t * prev_pos_tree;
  bool at_stream_end;		// stream_pos shows real end of file

  bool read_block() throw();

public:
  Matchfinder( const int dict_size, const int len_limit, const int ides )
    :
    partial_data_pos( 0 ),
    dictionary_size_( dict_size ),
    after_size( max_match_len ),
    buffer_size( ( 2 * std::max( 65536, dictionary_size_ ) ) +
                 max_num_trials + after_size ),
    buffer( new uint8_t[buffer_size] ),
    pos( 0 ),
    cyclic_pos( 0 ),
    stream_pos( 0 ),
    pos_limit( buffer_size - after_size ),
    ides_( ides ),
    match_len_limit_( len_limit ),
    prev_positions( new int32_t[num_prev_positions] ),
    at_stream_end( false )
    {
    if( !read_block() ) throw Error( "read error" );
    if( at_stream_end && stream_pos < dictionary_size_ )
      dictionary_size_ = std::max( min_dictionary_size, stream_pos );
    prev_pos_tree = new int32_t[2*dictionary_size_];
    for( int i = 0; i < num_prev_positions; ++i ) prev_positions[i] = -1;
    }

  ~Matchfinder()
    { delete[] prev_pos_tree; delete[] prev_positions; delete[] buffer; }

  uint8_t operator[]( const int i ) const throw() { return buffer[pos+i]; }
  int available_bytes() const throw() { return stream_pos - pos; }
  long long data_position() const throw() { return partial_data_pos + pos; }
  int dictionary_size() const throw() { return dictionary_size_; }
  bool finished() const throw() { return at_stream_end && pos >= stream_pos; }
  int match_len_limit() const throw() { return match_len_limit_; }
  const uint8_t * ptr_to_current_pos() const throw() { return buffer + pos; }

  bool dec_pos( const int ahead ) throw()
    {
    if( ahead < 0 || pos < ahead ) return false;
    pos -= ahead;
    cyclic_pos -= ahead;
    if( cyclic_pos < 0 ) cyclic_pos += dictionary_size_;
    return true;
    }

  int true_match_len( const int index, const int distance, int len_limit ) const throw()
    {
    if( index + len_limit > available_bytes() )
      len_limit = available_bytes() - index;
    const uint8_t * const data = buffer + pos + index - distance;
    int i = 0;
    while( i < len_limit && data[i] == data[i+distance] ) ++i;
    return i;
    }

  bool reset() throw();
  bool move_pos() throw();
  int longest_match_len( int * const distances = 0 ) throw();
  };


class Range_encoder
  {
  enum { buffer_size = 65536 };

  uint64_t low;
  long long partial_member_pos;
  uint8_t * const buffer;
  int pos;
  uint32_t range;
  int ff_count;
  const int odes_;
  uint8_t cache;

  void shift_low()
    {
    const uint32_t carry = low >> 32;
    if( low < 0xFF000000 || carry == 1 )
      {
      put_byte( cache + carry );
      for( ; ff_count > 0; --ff_count ) put_byte( 0xFF + carry );
      cache = low >> 24;
      }
    else ++ff_count;
    low = ( low & 0x00FFFFFF ) << 8;
    }

public:
  Range_encoder( const int odes )
    :
    low( 0 ),
    partial_member_pos( 0 ),
    buffer( new uint8_t[buffer_size] ),
    pos( 0 ),
    range( 0xFFFFFFFF ),
    ff_count( 0 ),
    odes_( odes ),
    cache( 0 ) {}

  ~Range_encoder() { delete[] buffer; }

  void flush_data()
    {
    if( pos > 0 )
      {
      const int wr = writeblock( odes_, (char *)buffer, pos );
      if( wr != pos ) throw Error( "write error" );
      partial_member_pos += pos;
      pos = 0;
      }
    }

  void flush() { for( int i = 0; i < 5; ++i ) shift_low(); }

  long long member_position() const throw()
    { return partial_member_pos + pos + ff_count; }

  void put_byte( const uint8_t b )
    {
    buffer[pos] = b;
    if( ++pos >= buffer_size ) flush_data();
    }

  void encode( const int symbol, const int num_bits )
    {
    for( int i = num_bits - 1; i >= 0; --i )
      {
      range >>= 1;
      if( (symbol >> i) & 1 ) low += range;
      if( range <= 0x00FFFFFF ) { range <<= 8; shift_low(); }
      }
    }

  void encode_bit( Bit_model & bm, const int bit )
    {
    const uint32_t bound = ( range >> bit_model_total_bits ) * bm.probability;
    if( !bit )
      {
      range = bound;
      bm.probability += (bit_model_total - bm.probability) >> bit_model_move_bits;
      }
    else
      {
      low += bound;
      range -= bound;
      bm.probability -= bm.probability >> bit_model_move_bits;
      }
    if( range <= 0x00FFFFFF ) { range <<= 8; shift_low(); }
    }

  void encode_tree( Bit_model bm[], const int symbol, const int num_bits )
    {
    int mask = ( 1 << ( num_bits - 1 ) );
    int model = 1;
    for( int i = num_bits; i > 0; --i, mask >>= 1 )
      {
      const int bit = ( symbol & mask );
      encode_bit( bm[model-1], bit );
      model <<= 1;
      if( bit ) model |= 1;
      }
    }

  void encode_tree_reversed( Bit_model bm[], int symbol, const int num_bits )
    {
    int model = 1;
    for( int i = num_bits; i > 0; --i )
      {
      const int bit = symbol & 1;
      encode_bit( bm[model-1], bit );
      model = ( model << 1 ) | bit;
      symbol >>= 1;
      }
    }

  void encode_matched( Bit_model bm[], int symbol, int match_byte )
    {
    int model = 1;
    for( int i = 7; i >= 0; --i )
      {
      const int bit = ( symbol >> i ) & 1;
      const int match_bit = ( match_byte >> i ) & 1;
      encode_bit( bm[(match_bit<<8)+model+0xFF], bit );
      model = ( model << 1 ) | bit;
      if( match_bit != bit )
        {
        while( --i >= 0 )
          {
          const int bit = ( symbol >> i ) & 1;
          encode_bit( bm[model-1], bit );
          model = ( model << 1 ) | bit;
          }
        break;
        }
      }
    }
  };


class Len_encoder
  {
  Bit_model choice1;
  Bit_model choice2;
  Bit_model bm_low[pos_states][len_low_symbols];
  Bit_model bm_mid[pos_states][len_mid_symbols];
  Bit_model bm_high[len_high_symbols];
  int prices[pos_states][max_len_symbols];
  const int len_symbols;
  int counters[pos_states];

  void update_prices( const int pos_state ) throw()
    {
    int * const pps = prices[pos_state];
    int price = price0( choice1 );
    int len = 0;
    for( ; len < len_low_symbols && len < len_symbols; ++len )
      pps[len] = price +
                 price_symbol( bm_low[pos_state], len, len_low_bits );
    price = price1( choice1 );
    for( ; len < len_low_symbols + len_mid_symbols && len < len_symbols; ++len )
      pps[len] = price + price0( choice2 ) +
                 price_symbol( bm_mid[pos_state], len - len_low_symbols, len_mid_bits );
    for( ; len < len_symbols; ++len )
      pps[len] = price + price1( choice2 ) +
                 price_symbol( bm_high, len - len_low_symbols - len_mid_symbols, len_high_bits );
    counters[pos_state] = len_symbols;
    }

public:
  Len_encoder( const int len_limit )
    : len_symbols( len_limit + 1 - min_match_len )
    {
    for( int i = 0; i < pos_states; ++i ) update_prices( i );
    }

  void encode( Range_encoder & range_encoder, int symbol,
               const int pos_state );

  int price( const int symbol, const int pos_state ) const throw()
    { return prices[pos_state][symbol - min_match_len]; }
  };


class Literal_encoder
  {
  Bit_model bm_literal[1<<literal_context_bits][0x300];

  int state( const int prev_byte ) const throw()
    { return ( prev_byte >> ( 8 - literal_context_bits ) ); }

public:
  void encode( Range_encoder & range_encoder, uint8_t prev_byte, uint8_t symbol )
    { range_encoder.encode_tree( bm_literal[state(prev_byte)], symbol, 8 ); }

  void encode_matched( Range_encoder & range_encoder, uint8_t prev_byte, uint8_t match_byte, uint8_t symbol )
    { range_encoder.encode_matched( bm_literal[state(prev_byte)], symbol, match_byte ); }

  int price_matched( uint8_t prev_byte, uint8_t symbol, uint8_t match_byte ) const throw()
    { return ::price_matched( bm_literal[state(prev_byte)], symbol, match_byte ); }

  int price_symbol( uint8_t prev_byte, uint8_t symbol ) const throw()
    { return ::price_symbol( bm_literal[state(prev_byte)], symbol, 8 ); }
  };


class LZ_encoder
  {
  enum { dis_align_mask = dis_align_size - 1,
         infinite_price = 0x0FFFFFFF,
         num_rep_distances = 4 };	// must be 4

  struct Trial
    {
    State state;
    int dis;
    int prev_index;
    int price;		// dual use var; cumulative price, match length
    int reps[num_rep_distances];
    void update( const int d, const int p_i, const int pr ) throw()
      { if( pr < price ) { dis = d; prev_index = p_i; price = pr; } }
    };

  int longest_match_found;
  uint32_t crc_;

  Bit_model bm_match[State::states][pos_states];
  Bit_model bm_rep[State::states];
  Bit_model bm_rep0[State::states];
  Bit_model bm_rep1[State::states];
  Bit_model bm_rep2[State::states];
  Bit_model bm_len[State::states][pos_states];
  Bit_model bm_dis_slot[max_dis_states][1<<dis_slot_bits];
  Bit_model bm_dis[modeled_distances-end_dis_model];
  Bit_model bm_align[dis_align_size];

  Matchfinder & matchfinder;
  Range_encoder range_encoder;
  Len_encoder len_encoder;
  Len_encoder rep_match_len_encoder;
  Literal_encoder literal_encoder;

  const int num_dis_slots;
  int match_distances[max_match_len+1];
  Trial trials[max_num_trials];

  int dis_slot_prices[max_dis_states][2*max_dictionary_bits];
  int dis_prices[max_dis_states][modeled_distances];
  int align_prices[dis_align_size];
  int align_price_count;

  void fill_align_prices() throw();
  void fill_distance_prices() throw();

  uint32_t crc() const throw() { return crc_ ^ 0xFFFFFFFF; }

  void mtf_reps( const int dis, int reps[num_rep_distances] ) throw()
    {
    if( dis >= num_rep_distances )
      {
      for( int i = num_rep_distances - 1; i > 0; --i ) reps[i] = reps[i-1];
      reps[0] = dis - num_rep_distances;
      }
    else if( dis > 0 )
      {
      const int distance = reps[dis];
      for( int i = dis; i > 0; --i ) reps[i] = reps[i-1];
      reps[0] = distance;
      }
    }

  int price_rep_len1( const State & state, const int pos_state ) const throw()
    {
    return price0( bm_rep0[state()] ) + price0( bm_len[state()][pos_state] );
    }

  int price_rep( const int rep, const int len, const State & state,
                 const int pos_state ) const throw()
    {
    int price = rep_match_len_encoder.price( len, pos_state );
    if( rep == 0 )
      {
      price += price0( bm_rep0[state()] );
      price += price1( bm_len[state()][pos_state] );
      }
    else
      {
      price += price1( bm_rep0[state()] );
      if( rep == 1 )
        price += price0( bm_rep1[state()] );
      else
        {
        price += price1( bm_rep1[state()] );
        price += price_bit( bm_rep2[state()], rep - 2 );
        }
      }
    return price;
    }

  int price_pair( const int dis, const int len, const int pos_state ) const throw()
    {
    if( len <= min_match_len && dis >= modeled_distances )
      return infinite_price;
    int price = len_encoder.price( len, pos_state );
    const int dis_state = get_dis_state( len );
    if( dis < modeled_distances )
      price += dis_prices[dis_state][dis];
    else
      price += dis_slot_prices[dis_state][dis_slots[dis]] +
               align_prices[dis & dis_align_mask];
    return price;
    }

  void encode_pair( const uint32_t dis, const int len, const int pos_state ) throw()
    {
    len_encoder.encode( range_encoder, len, pos_state );
    const int dis_slot = dis_slots[dis];
    range_encoder.encode_tree( bm_dis_slot[get_dis_state(len)], dis_slot, dis_slot_bits );

    if( dis_slot >= start_dis_model )
      {
      const int direct_bits = ( dis_slot >> 1 ) - 1;
      const uint32_t base = ( 2 | ( dis_slot & 1 ) ) << direct_bits;
      const uint32_t direct_dis = dis - base;

      if( dis_slot < end_dis_model )
        range_encoder.encode_tree_reversed( bm_dis + base - dis_slot,
            direct_dis, direct_bits );
      else
        {
        range_encoder.encode( direct_dis >> dis_align_bits, direct_bits - dis_align_bits );
        range_encoder.encode_tree_reversed( bm_align, direct_dis, dis_align_bits );
        if( --align_price_count <= 0 ) fill_align_prices();
        }
      }
    }

  int read_match_distances() throw()
    {
    int len = matchfinder.longest_match_len( match_distances );
    if( len == matchfinder.match_len_limit() )
      len += matchfinder.true_match_len( len, match_distances[len] + 1, max_match_len - len );
    return len;
    }

  bool move_pos( int n, bool skip = false ) throw()
    {
    while( --n >= 0 )
      {
      if( skip ) skip = false;
      else matchfinder.longest_match_len();
      if( !matchfinder.move_pos() ) return false;
      }
    return true;
    }

  void backward( int cur )
    {
    int & dis = trials[cur].dis;
    while( cur > 0 )
      {
      const int prev_index = trials[cur].prev_index;
      Trial & prev_trial = trials[prev_index];
      std::swap( dis, prev_trial.dis );
      prev_trial.price = cur - prev_index;		// len
      cur = prev_index;
      }
    }

  int best_pair_sequence( const int reps[num_rep_distances],
                          const State & state );

  void flush( const State & state );

public:
  LZ_encoder( Matchfinder & mf, const File_header & header, const int odes );

  bool encode_member( const long long member_size );

  long long member_position() const throw()
    { return range_encoder.member_position(); }
  };
