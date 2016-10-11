LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

entity UART_VHDL is
   port (
        clock:     in  std_logic;
        reset_N:   in  std_logic;

        address:   in  std_logic;
        writeData: in  std_logic_vector(7 downto 0);
        write:     in  std_logic;
        readData:  out std_logic_vector(7 downto 0);
        read:      in  std_logic;

        serialIn:  in  std_logic;
        serialOut: out std_logic
        );
end UART_VHDL;

ARCHITECTURE RTL OF UART_VHDL IS

    type   t_retime is record
                       once:    std_logic;
                       twice:   std_logic;
                       end record;

    type   t_TxState  is (
                         IDLE,
                         START,
                         SEND,
                         STOP
                         );

    type   t_RxState  is (
                         IDLE,
                         START,
                         RECEIVE,
                         STOP,
                         FULL,
                         ERROR
                         );

    constant c_TX:        std_logic := '0';
    constant c_RX:        std_logic := '0';
    constant c_STATUS:    std_logic := '1';

    constant RxReady:     integer := 0;
    constant RxError:     integer := 1;
    constant TxReady:     integer := 2;

    signal   s_serialOut: std_logic;
    signal   s_readData:  std_logic_vector(7 downto 0);

begin

    --+++++++++++++++++++++++++++
    --                          +
    --   Main clocked logic.    +
    --                          +
    --+++++++++++++++++++++++++++

clockedLogic: process (clock, reset_N)

    variable RxState:          t_RxState;
    variable TxState:          t_TxState;
    variable serialInRetimed:  t_retime;

    variable Tx,
             Rx:                 std_logic_vector(7 downto 0);

    variable RxSampleCount,
             RxBitCount,
             TxSampleCount,
             TxBitCount:         integer range 0 to 8;

    variable v_serialOut:        std_logic;
    variable v_readData:         std_logic_vector(7 downto 0);

    begin

    if reset_N = '0' then

          s_serialOut     <= '1';

          RxState         := IDLE;
          TxState         := IDLE;

          Tx              := (others=>'0');
          Rx              := (others=>'0');

          serialInRetimed := ('1','1');
          RxSampleCount   := 0;
          RxBitCount      := 0;
          TxSampleCount   := 0;
          TxBitCount      := 0;

    elsif clock'event and clock='1' then

        --++++++++++++++++++++++++++++++++++++++++++++++++
        --                                               +
        --   Assign all temporary variables in order to  +
        --   avoid generating unwanted flip-flops.       +
        --                                               +
        --++++++++++++++++++++++++++++++++++++++++++++++++

        v_serialOut := s_serialOut;
        v_readData  := (others=>'0');

        --+++++++++++++++++++++++++++++
        --                            +
        --   Read/Write Registers.    +
        --                            +
        --+++++++++++++++++++++++++++++

        if write = '1' then
            case address is

                when c_TX      => Tx := writeData;
                                  TxState := START;

                when others	   =>
            end case;
        end if;

        if read = '1' then
            case address is

                when c_RX      => v_readData := Rx;
                                    RxState  := IDLE;

                when c_STATUS  => if RxState=FULL then
                                    v_readData(RxReady) := '1';
                                  end if;

                                  if RxState=ERROR then
                                    v_readData(RxError) := '1';
                                      RxState  := IDLE;
                                  end if;

                                  if TxState=IDLE then
                                    v_readData(TxReady) := '1';
                                  end if;

                when others	   =>
            end case;
        end if;

        --+++++++++++++++++++++++++++++++
        --                              +
        --   Transmit State Machine.    +
        --                              +
        --+++++++++++++++++++++++++++++++

        case TxState is

            when IDLE  =>

            when START => v_SerialOut     := '0';              -- Start Bit.
                            TxSampleCount := 0;
                            TxBitCount    := 0;
                            TxState       := SEND;

            when SEND  =>   TxSampleCount := TxSampleCount+1;  -- Eight Data Bits.
                            if TxSampleCount = 8 then
                                TxSampleCount := 0;
                              v_SerialOut := Tx(TxBitCount);
                                TxBitCount := TxBitCount+1;
                                if TxBitCount=8 then
                                    TxState := STOP;
                                end if;
                            end if;

            when STOP  =>   TxSampleCount := TxSampleCount+1;
                            if TxSampleCount = 8 then
                              v_SerialOut := '1';	       -- Stop Bit.
                                TxState := IDLE;
                            end if;

        end case;

        --++++++++++++++++++++++++++++++
        --                             +
        --   Receive State Machine.    +
        --                             +
        --++++++++++++++++++++++++++++++

        case RxState is

            when IDLE    => if SerialInRetimed.TWICE = '0' then		   -- Falling Edge of Start Bit.
                                RxSampleCount := 0;
                                RxBitCount    := 0;
                                RxState := RECEIVE;
                            end if;

            when START   => RxSampleCount := RxSampleCount+1;
                            if RxSampleCount = 4 then			   -- Centre of Start Bit
                                RxState := RECEIVE;
                            end if;

            when RECEIVE => RxSampleCount := RxSampleCount+1;	           -- Eight Data Bits.
                            if RxSampleCount = 8 then
                                RxSampleCount := 0;
                                Rx(RxBitCount)  := serialInRetimed.TWICE;
                                RxBitCount    := RxBitCount+1;
                                if RxBitCount=8 then
                                    RxState := STOP;
                                end if;
                            end if;

            when STOP    => RxSampleCount := RxSampleCount+1;
                            if RxSampleCount = 8 then
                                if serialInRetimed.TWICE = '1' then       -- Check Stop Bit
                                    RxState := FULL;
                                else
                                    RxState := ERROR;
                                end if;
                            end if;

            when FULL    =>

            when ERROR   =>

        end case;

        --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        --                                                          +
        --   Re-time any asynchronous signals before use in order   +
        --   to avoid race hazards or problems of metastability     +
        --                                                          +
        --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        serialInRetimed.TWICE := serialInRetimed.ONCE;
        serialInRetimed.ONCE  := serialIn;

        --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        --                                                                  +
        --   Assign the temporary variables to their `real' counterparts.   +
        --                                                                  +
        --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        s_serialOut <= v_serialOut;
        s_readData  <= v_readData;

    end if;

end process clockedLogic;

    --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    --                                                                 +
    --   Assign the outputs from their associated internal signals.    +
    --                                                                 +
    --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

serialOut <= s_serialOut;
readData  <= s_readData;

end RTL;
