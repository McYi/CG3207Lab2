
----------------------------------------------------------------------------------
-- Company: NUS
-- Engineer: Rajesh Panicker
-- 
-- Create Date:   10:39:18 13/09/2014
-- Design Name: 	ALU
-- Target Devices: Nexys 4 (Artix 7 100T)
-- Tool versions: ISE 14.7
-- Description: ALU template for MIPS processor
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------

------------------------------------------------------------------
-- ALU Entity
------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity alu is
generic (width	: integer := 32);
Port (Clk		: in	STD_LOGIC;
		Control	: in	STD_LOGIC_VECTOR (5 downto 0);
		Operand1	: in	STD_LOGIC_VECTOR (width-1 downto 0);
		Operand2	: in	STD_LOGIC_VECTOR (width-1 downto 0);
		Result1	: out	STD_LOGIC_VECTOR (width-1 downto 0);
		Result2	: out	STD_LOGIC_VECTOR (width-1 downto 0);
		Status	: out	STD_LOGIC_VECTOR (2 downto 0); -- busy (multicycle only), overflow (add and sub), zero (sub)
		Debug		: out	STD_LOGIC_VECTOR (width-1 downto 0));		
end alu;

------------------------------------------------------------------
-- ALU Architecture
------------------------------------------------------------------
architecture Behavioral of alu is

type states is (COMBINATIONAL, MULTI_CYCLE);
signal state, n_state	: states := COMBINATIONAL;

----------------------------------------------------------------------------
-- Adder instantiation
----------------------------------------------------------------------------
component adder is
generic (width : integer);
port (A 		: in 	STD_LOGIC_VECTOR(width-1 downto 0);
		B 		: in 	STD_LOGIC_VECTOR(width-1 downto 0);
		C_in 	: in 	STD_LOGIC;
		S 		: out STD_LOGIC_VECTOR(width-1 downto 0);
		C_out	: out STD_LOGIC);
end component adder;

----------------------------------------------------------------------------
-- Shifter instantiation
----------------------------------------------------------------------------
component shifter is
generic (width : integer);
port (O1 		: in STD_LOGIC_VECTOR(width-1 downto 0);
		O2 		: in STD_LOGIC_VECTOR(4 downto 0);
	   C1		   : in	STD_LOGIC_VECTOR (5 downto 0);
		R1 		: out STD_LOGIC_VECTOR(width-1 downto 0));
end component shifter;

----------------------------------------------------------------------------
-- Adder signals
----------------------------------------------------------------------------
signal B 		: STD_LOGIC_VECTOR(width-1 downto 0) := (others => '0'); 
signal C_in 	: STD_LOGIC := '0';
signal S 		: STD_LOGIC_VECTOR(width-1 downto 0) := (others => '0'); 
signal C_out	: STD_LOGIC := '0'; --not used

----------------------------------------------------------------------------
-- Shifter signals
----------------------------------------------------------------------------
signal O1 		: STD_LOGIC_VECTOR(width-1 downto 0) := (others => '0'); 
signal O2 	   : STD_LOGIC := '0';
signal C1 		: STD_LOGIC_VECTOR(5 downto 0) := (others => '0'); 
signal R1	   : STD_LOGIC_VECTOR(width-1 downto 0) := (others => '0'); --used

----------------------------------------------------------------------------
-- Signals for MULTI_CYCLE_PROCESS
----------------------------------------------------------------------------
signal Result1_multi		: STD_LOGIC_VECTOR (width-1 downto 0) := (others => '0'); 
signal Result2_multi		: STD_LOGIC_VECTOR (width-1 downto 0) := (others => '0');
signal Debug_multi		: STD_LOGIC_VECTOR (width-1 downto 0) := (others => '0'); 
signal done		 			: STD_LOGIC := '0';

begin

-- <port maps>
adder32 : adder generic map (width =>  width) port map (  A=>Operand1, B=>B, C_in=>C_in, S=>S, C_out=>C_out );
shifter32: shifter generic map (width =>  width) port map ( O1=>Operand1, O2=>Operand2(4 downto 0), C1=>Control,R1 => R1);
-- </port maps>

----------------------------------------------------------------------------
-- COMBINATIONAL PROCESS
----------------------------------------------------------------------------
COMBINATIONAL_PROCESS : process (
											Control, Operand1, Operand2, state, -- external inputs
											S, -- ouput from the adder (or other components)
											R1, -- output from the shifter
											Result1_multi, Result2_multi, Debug_multi, done -- from multi-cycle process(es)
											)
variable temp_slt: STD_LOGIC_VECTOR(2 downto 0):= (others => '0');
variable temp_sltu: STD_LOGIC_VECTOR(1 downto 0):= (others => '0');	

begin

-- <default outputs>
Status(2 downto 0) <= "000"; -- both statuses '0' by default 
Result1 <= (others=>'0');
Result2 <= (others=>'0');
Debug <= (others=>'0');

n_state <= state;

B <= Operand2;
C_in <= '0';
-- </default outputs>

--reset
if Control(5) = '1' then
	n_state <= COMBINATIONAL;
else

case state is
	when COMBINATIONAL =>
		case Control(4 downto 0) is
		--and
		when "00000" =>   -- takes 0 cycles to execute
			Result1 <= Operand1 and Operand2;
		--or
		when "00001" =>
			Result1 <= Operand1 or Operand2;
		--nor
		when "01100" => 
			Result1 <= Operand1 nor Operand2;
		--add
		when "00010" =>
			Result1 <= S;
			-- overflow
			Status(1) <= ( Operand1(width-1) xnor  Operand2(width-1) )  and ( Operand2(width-1) xor S(width-1) );
		-- sub
		when "00110" =>
			B <= not(Operand2);
			C_in <= '1';
			Result1 <= S;
			-- overflow
		   Status(1) <=( Operand1(width-1) xnor  Operand2(width-1) )  and not( Operand2(width-1) xor S(width-1));
		-- xor
		when "00100" =>
          Result1	<= Operand1 xor Operand2;	
		-- shift
	   when "00101"|"01101"|"01001" =>
			 Result1 <= R1;
      -- slt
		when "00111" =>  
		   B <= not(Operand2);
			C_in <= '1';
			temp_slt := Operand1(width-1)&Operand2(width-1)&S(width -1);
     		case temp_slt(2 downto 0) is
				when "000" |"110"| "010"| "011"=> Result1 <= (others => '0');
				when others => Result1 <= x"00000001";
		   end case;
	   -- sltu		
		when "01110" => 
         B <= not(Operand2);
			C_in <= '1';	
			temp_sltu := Operand1(width -1)& S(width-1);
			if (Operand1 xor Operand2) = x"00000000" then 
				Result1 <= (others => '0');
			else 
				case temp_sltu(1 downto 0) is
					when "00" | "11" => Result1 <= (others => '0');             
					when others => Result1 <= x"00000001";         
           end case;
			end if; 	
			--zero
			if S = x"00000000" then 
				Status(0) <= '1'; 
			else
				Status(0) <= '0';
			end if;
		-- multi-cycle operations
		when "10000"|"11110"|"10001"|"10010"|"10011" => 
			n_state <= MULTI_CYCLE;
			Status(2) <= '1';
		-- default cases (already covered)
		when others=> null;
		end case;
	when MULTI_CYCLE => 
		if done = '1' then
			Result1 <= Result1_multi;
			Result2 <= Result2_multi;
			Debug <= Debug_multi;
			n_state <= COMBINATIONAL;
			Status(2) <= '0';
		else
			Status(2) <= '1';
			n_state <= MULTI_CYCLE;
		end if;
	end case;
end if;	
end process;


----------------------------------------------------------------------------
-- STATE UPDATE PROCESS
----------------------------------------------------------------------------

STATE_UPDATE_PROCESS : process (Clk) -- state updating
begin  
   if (Clk'event and Clk = '1') then
		state <= n_state;
   end if;
end process;


----------------------------------------------------------------------------
-- MULTI CYCLE PROCESS
----------------------------------------------------------------------------


MULTI_CYCLE_PROCESS : process (Clk) -- multi-cycle operations done here
-- assume that Operand1 and Operand 2 do not change while multi-cycle operations are being performed


variable count : natural := 0;

-- variables for unsigned multiplication
variable temp1, temp2     		: 	STD_LOGIC_VECTOR(width-1 downto 0) := (others => '0');
variable temp33 					: 	STD_LOGIC_VECTOR(width downto 0) := (others => '0');
variable temp_sum 		: 	STD_LOGIC_VECTOR(2*width-1 downto 0) := (others => '0');

-- variables for signed multiplication
variable temp65, booth_A, booth_P, booth_S	:STD_LOGIC_VECTOR(2*width downto 0) := (others => '0');

-- variable for division
variable remainder 		: 	STD_LOGIC_VECTOR(2*width downto 0) := (others => '0');

-- variable for signed division
variable temp3 	: 	STD_LOGIC_VECTOR(width-1 downto 0) := (others => '0');
variable sign 				:	STD_LOGIC := '0';
variable temp4 		: 	STD_LOGIC_VECTOR(2*width downto 0) := (others => '0');

begin  
   if (Clk'event and Clk = '1') then 
		if Control(5) = '1' then
			count := 0;
			temp_sum := (others => '0');
		end if;
		done <= '0';
		if n_state = MULTI_CYCLE then
			case Control(4 downto 0) is
			-- unsigned multiplication
			when "10001" => 			
				if count = 0 then
					temp1 := Operand1;
					temp2 := Operand2;
					temp33 := (others => '0');
					temp_sum := (others => '0');
				end if;	
				if temp2(count) = '1' then 
					temp33 := temp33 + ('0' & temp1);	
				end if;
				
				temp_sum(count) := temp33(0);
				temp33 := '0' & temp33(width downto 1);
				
				count := count+1;	
				
				if count=32 then	
					Result1_multi <= temp_sum(width-1 downto 0);
					Result2_multi <= temp33(width-1 downto 0);
					Debug_multi <= Operand1(width/2-1 downto 0) & Operand2(width/2-1 downto 0); -- just a random output
					temp_sum := (others => '0');
					done <= '1';
					count := 0;
				end if;

			-- signed multiplication (booth multiplication algorithm)
			when "10000" => 
				if count = 0 then
					booth_A(2*width downto width+1) 	:= operand1;
					booth_P 									:= (others => '0');
					booth_P(width downto 1)				:= operand2;
					booth_S(width*2 downto width+1) 	:= (not (Operand1) + '1');
					temp65 									:= (others => '0');
				end if;
				
				if booth_P(1 downto 0) = "01" then
					temp65 := booth_A + booth_P;
				elsif booth_P(1 downto 0) = "10" then
					temp65 := booth_P + booth_S;
				else
					temp65 := booth_P;
				end if;
				
				booth_P := temp65(2*width) & temp65(2*width downto 1);
				count := count + 1;
				
				if count = 32 then	
					Result1_multi <= booth_P(width downto 1);
					Result2_multi <= booth_P(2*width downto width + 1);
					Debug_multi <= Operand1(width/2-1 downto 0) & Operand2(width/2-1 downto 0); -- just a random output
					done <= '1';
					count := 0;
				end if;
				
			-- unsigned division
			when "10011" =>    
				if count = 0 then
					remainder := (others => '0');
					remainder(width-1 downto 0) := Operand1;
				else
					temp65 := remainder;
					remainder(2*width downto width) := remainder (2*width downto width) - ('0' & Operand2);
					
					if remainder(2*width) = '0' then
						remainder := remainder(2*width-1 downto 0) & '1';
					else
						remainder := temp65(2*width-1 downto 0) & '0';
					end if;	
				end if;
				
				if count = 33 then	
					Result1_multi <= remainder(width-1 downto 0);
					Result2_multi <= remainder(2*width downto width + 1);
					Debug_multi <= Operand1(width/2-1 downto 0) & Operand2(width/2-1 downto 0); -- just a random output
					done <= '1';
					count := 0;
				end if;
				
				count := count + 1;
				
			-- signed division
			when "10010" => 
				if count = 0 then
					remainder := (others => '0');
					if Operand1(width-1) = '0' then
						remainder(width downto 1) := Operand1;
					else
						remainder(width downto 1) := (not(Operand1) + 1);
					end if;
					if Operand2(width-1) = '0' then
						temp3 := Operand2;
					else
						temp3 := (not(Operand2) + 1);
					end if;
					sign := Operand1(width-1) xor Operand2(width-1);
					temp4 := remainder;
				else
					temp65 := remainder;
					remainder(2*width-1 downto width) := remainder (2*width-1 downto width) -  temp3;
					
					if remainder(2*width-1) = '0' then
						remainder := remainder(2*width-1 downto 0) & '1';
					else
						remainder := temp65(2*width-1 downto 0) & '0';
					end if;
					
					if sign = '1' then
						temp4(width-1 downto 0) := (not(remainder(width-1 downto 0)) + 1);
						temp4(2*width downto width+1) := (not(remainder(2*width downto width+1)) + 1);
					else
						temp4 := remainder;
					end if;

				end if;	
				
				if count = 32 then
					Result1_multi <= temp4(width-1 downto 0);
					Result2_multi <= temp4(2*width downto width+1); 
					Debug_multi <= temp4(width downto 1); -- just a random output
					done <= '1';
					count := 0;
				end if;
				
				count := count + 1;
					
			when "11110" => -- takes 1 cycle to execute, just returns the operands
				if state = COMBINATIONAL then
					Result1_multi <= Operand1;
					Result2_multi <= Operand2;
					Debug_multi <= Operand1(width-1 downto width/2) & Operand2(width-1 downto width/2);
					done <= '1';
				end if;	
			when others=> null;
			end case;
		end if;
	end if;
end process;


end Behavioral;

------------------------------------------------------------------
-- Adder Entity
------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity adder is
generic (width : integer := 32);
port (A 		: in STD_LOGIC_VECTOR(width-1 downto 0);
		B 		: in STD_LOGIC_VECTOR(width-1 downto 0);
		C_in 	: in STD_LOGIC;
		S 		: out STD_LOGIC_VECTOR(width-1 downto 0);
		C_out	: out STD_LOGIC);
end adder;

------------------------------------------------------------------
-- Adder Architecture
------------------------------------------------------------------
architecture adder_arch of adder is
signal S_wider : STD_LOGIC_VECTOR(width downto 0);
begin
	S_wider <= ('0'& A) + ('0'& B) + C_in;
	S <= S_wider(width-1 downto 0);
	C_out <= S_wider(width);
end adder_arch;

------------------------------------------------------------------
-- Shifter Entity
------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity shifter is
generic (width : integer := 32);
port (O1	: in STD_LOGIC_VECTOR(width-1 downto 0);
		O2	: in STD_LOGIC_VECTOR(4 downto 0);
	   C1	: in	STD_LOGIC_VECTOR (5 downto 0);
		R1	: out STD_LOGIC_VECTOR(width-1 downto 0));
end shifter;

------------------------------------------------------------------
-- Shifter Architecture
------------------------------------------------------------------
architecture shifter_arch of shifter is

component sllShifter is
	generic (shiftBit : integer);
	port(input : in STD_LOGIC_VECTOR(width-1 downto 0);
	     enable: in STD_LOGIC;
		  output: out STD_LOGIC_VECTOR(width-1 downto 0));
end component;

component srlShifter is
	generic (shiftBit : integer);
	port(input : in STD_LOGIC_VECTOR(width-1 downto 0);
	     enable: in STD_LOGIC;
		  output: out STD_LOGIC_VECTOR(width-1 downto 0));
end component;

component sraShifter is
	generic (shiftBit : integer);
	port(input : in STD_LOGIC_VECTOR(width-1 downto 0);
	     enable: in STD_LOGIC;
		  output: out STD_LOGIC_VECTOR(width-1 downto 0));
end component;

signal output_sll_1 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_sll_2 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_sll_4 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_sll_8 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_sll_16: STD_LOGIC_VECTOR(width-1 downto 0);

signal output_srl_1 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_srl_2 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_srl_4 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_srl_8 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_srl_16: STD_LOGIC_VECTOR(width-1 downto 0);

signal output_sra_1 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_sra_2 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_sra_4 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_sra_8 : STD_LOGIC_VECTOR(width-1 downto 0);
signal output_sra_16: STD_LOGIC_VECTOR(width-1 downto 0);

begin  
	sll_1 : sllShifter generic map(shiftBit => 1) port map(O1,O2(0),output_sll_1);
	sll_2 : sllShifter generic map(shiftBit => 2) port map(output_sll_1,O2(1),output_sll_2);
	sll_4 : sllShifter generic map(shiftBit => 4) port map(output_sll_2,O2(2),output_sll_4);
	sll_8 : sllShifter generic map(shiftBit => 8) port map(output_sll_4,O2(3),output_sll_8);
	sll_16: sllShifter generic map(shiftBit => 16) port map(output_sll_8,O2(4),output_sll_16);
	
	srl_1 : srlShifter generic map(shiftBit => 1) port map(O1,O2(0),output_srl_1);
	srl_2 : srlShifter generic map(shiftBit => 2) port map(output_srl_1,O2(1),output_srl_2);
	srl_4 : srlShifter generic map(shiftBit => 4) port map(output_srl_2,O2(2),output_srl_4);
	srl_8 : srlShifter generic map(shiftBit => 8) port map(output_srl_4,O2(3),output_srl_8);
	srl_16: srlShifter generic map(shiftBit => 16) port map(output_srl_8,O2(4),output_srl_16);
	
	sra_1 : sraShifter generic map(shiftBit => 1) port map(O1,O2(0),output_sra_1);
	sra_2 : sraShifter generic map(shiftBit => 2) port map(output_sra_1,O2(1),output_sra_2);
	sra_4 : sraShifter generic map(shiftBit => 4) port map(output_sra_2,O2(2),output_sra_4);
	sra_8 : sraShifter generic map(shiftBit => 8) port map(output_sra_4,O2(3),output_sra_8);
	sra_16: sraShifter generic map(shiftBit => 16) port map(output_sra_8,O2(4),output_sra_16);
		
	with C1(4 downto 0) select 	    
	    R1 <= output_sll_16 when "00101", --SLL 
				 output_srl_16 when "01101", --SRL
				 output_sra_16 when others; --SRA
				 
end shifter_arch;

------------------------------------------------------------------
-- sllShifter Entity
------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity sllshifter is
generic (shiftBit : integer;
			width : integer := 32);
	port(input : in STD_LOGIC_VECTOR(width-1 downto 0);
	     enable: in STD_LOGIC;
		  output: out STD_LOGIC_VECTOR(width-1 downto 0));
end sllshifter;

------------------------------------------------------------------
-- sllShifter Architecture
------------------------------------------------------------------
architecture sllshifter_arch of sllshifter is
signal zeroArray : STD_LOGIC_VECTOR(width-1 downto 0) := (others => '0');
begin
with enable select 
	output <= input(width-1-shiftBit downto 0)&zeroArray(shiftBit-1 downto 0) when '1',
				 input when others;
end sllshifter_arch;

------------------------------------------------------------------
-- srlShifter Entity
------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity srlshifter is
generic (shiftBit : integer;
			width : integer := 32);
	port(input : in STD_LOGIC_VECTOR(width-1 downto 0);
	     enable: in STD_LOGIC;
		  output: out STD_LOGIC_VECTOR(width-1 downto 0));
end srlshifter;

------------------------------------------------------------------
-- srlShifter Architecture
------------------------------------------------------------------
architecture srlshifter_arch of srlshifter is
signal zeroArray : STD_LOGIC_VECTOR(width-1 downto 0) := (others=>'0');
begin
with enable select 
	output <= zeroArray(shiftBit-1 downto 0)&input(width-1 downto shiftBit) when '1',
				 input when others;
end srlshifter_arch;

------------------------------------------------------------------
-- sraShifter Entity
------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity srashifter is
generic (shiftBit : integer;
			width : integer := 32);
	port(input : in STD_LOGIC_VECTOR(width-1 downto 0);
	     enable: in STD_LOGIC;
		  output: out STD_LOGIC_VECTOR(width-1 downto 0));
end srashifter;

------------------------------------------------------------------
-- sraShifter Architecture
------------------------------------------------------------------
architecture srashifter_arch of srashifter is
signal zeroArray : STD_LOGIC_VECTOR(width-1 downto 0) := (others=>'0');
signal oneArray  : STD_LOGIC_VECTOR(width-1 downto 0) := (others=>'1');
signal temp: STD_LOGIC_VECTOR(1 downto 0);
begin
temp <= enable&input(width-1);
with temp select
	output <= zeroArray(shiftBit-1 downto 0)&input(width-1 downto shiftBit) when "10",
	          oneArray(shiftBit-1 downto 0)&input(width-1 downto shiftBit) when "11",
				 input when others;
end srashifter_arch;




