library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.math_utility_pkg.all;
use ieee_proposed.fixed_pkg.all;
use ieee_proposed.float_pkg.all;

use std.textio.all;

entity fix is
    generic (
        iwidth  : integer := 3;
        ifix    : integer := 1;
        owidth  : integer := 4;
        ofix    : integer := 2
    );
end entity;

architecture test of fix is

    constant ihigh : integer := iwidth - ifix - 1;
    constant ilow : integer := -ifix;

    constant ohigh : integer := owidth - ofix - 1;
    constant olow : integer := -ofix;
    
begin

    process
        variable ivec : SFixed(ihigh downto ilow);
        variable ovec : SFixed(ohigh downto olow);
        
        variable l : line;
        procedure write(
            u : SFixed
        ) is 
            variable l : line;
        begin
            write(l, u);
            write(l, string'("     ("));
            write(l, to_real(u));
            write(l, string'(")"));
            writeline(output, l);
        end procedure;
        variable f : real;
        
        function pow2(i : integer) return real is
            variable r : real := 1.0;
        begin
            if i = 0 then
                return 1.0;
            elsif i > 0 then
                return real(2 ** i);
            else
                for j in 0 downto i+1 loop
                    r := r / 2.0;
                end loop;
                return r;
            end if;
        end function;
        
    begin

        for i in 0 to (2 ** iwidth) - 1 loop
            --f := real(i) / (pow2(ifix));
            f := (real(i) - pow2(iwidth-1)) / (pow2(ifix));
            ivec := to_SFixed(f, ihigh, ilow);
            ovec := resize(ivec, ohigh, olow);
            write(ovec);
        end loop;
    
        wait;
    end process;

end architecture;

