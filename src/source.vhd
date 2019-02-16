----------------------------------------------------------------------------------
-- Company: Politecnico di Milano
-- Engineer: Stefano Formicola 847762, Leonardo Guerra 844624

-- Description: Project Reti Logiche 

-- Additional Comments:
-- Manhattan Distance = |x1-x2|+|y1-y2|
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity project_reti_logiche is
	port (
		i_clk : in std_logic;
		i_start : in std_logic;
		i_rst : in std_logic;
		i_data : in std_logic_vector(7 downto 0);
		o_address : out std_logic_vector(15 downto 0);
		o_done : out std_logic;
		o_en : out std_logic;
		o_we : out std_logic;
		o_data : out std_logic_vector (7 downto 0)
	);
end project_reti_logiche;


architecture Behavioral of project_reti_logiche is

	constant inMaskAddress : std_logic_vector (15 downto 0) := std_logic_vector(to_unsigned(0,16));
	constant pointXAddress : std_logic_vector (15 downto 0) := std_logic_vector(to_unsigned(17,16));
	constant pointYAddress : std_logic_vector (15 downto 0) := std_logic_vector(to_unsigned(18,16));
	constant outMaskAddress : std_logic_vector (15 downto 0) := std_logic_vector(to_unsigned(19,16));
	constant defaultAddress : std_logic_vector (15 downto 0) := std_logic_vector(to_unsigned(0,16));
	constant defaultValue : std_logic_vector (7 downto 0) := std_logic_vector(to_unsigned(0,8));
	constant defaultDist : integer := 510; --|255-0|+|255-0|=510
	constant one : unsigned (7 downto 0) := "00000001";

	type state_t is (idle, reset, start, processing, ending, done);
	type init_t is (idle, readInMask, readX, readY);
	type end_t is (idle, writeMask);
	type computation_t is (idle, checkMask, getCentroidX, getCentroidY, evaluateDist, modifyOutMask);

	signal fsmState : state_t := idle;
	signal startState : init_t := idle;
	signal endState : end_t := idle;
	signal executionState : computation_t := idle;
	
    signal centroidIndex : integer range 0 to 8 := 0;
            -- e.g. centroid1 has centroidIndex = 0; ...
            -- incremented in checkMask (if inMask(centroidIndex) = 0) or in evaluateDist (if current distance > minDist) or in modifyOutMask (if current distance <= minDist);
            -- if centroidIndex = 8 then fsmState switches from processing to ending state
	signal oneHotIndex : std_logic_vector (7 downto 0) := "00000000";
		-- vector that corresponds to the one-hot code of the centroid index,
		-- created in evaluateDist with the left logic shift between "00000001"
		-- and the index of the centroid
			-- e.g. centroid3 <=> oneHotIndex="00000100"
	signal outputMask : std_logic_vector (7 downto 0) := "00000000";
		-- if current distance < minDist then the outputMask corresponds to oneHotIndex
		-- of the centroid, else if it is equal to the minDist 
		-- it becomes the OR between its value and oneHotIndex
    
begin
	process (i_clk, i_rst) is
        variable inputMask  : std_logic_vector (7 downto 0) := "00000000";
        variable pointX     : integer range 0 to 255 := 0;
        variable pointY     : integer range 0 to 255 := 0;
        variable centroidX  : integer range 0 to 255 := 0;
        variable centroidY  : integer range 0 to 255 := 0;
		variable partialDist: integer range 0 to defaultDist := 0;
		variable minDist    : integer range 0 to defaultDist := defaultDist; --initialized to the maximum value

	begin
        if(i_rst = '1') then
            fsmState <= reset;
        end if;
	
		if(rising_edge(i_clk)) then
		
			case fsmState is

				when reset=>
					startState <= idle;
					executionState <= idle;
					endState <= idle;
                    fsmState <= idle;
                    
					inputMask := defaultValue;
					pointX := 0;
					pointY := 0;
					centroidIndex <= 0;
					centroidX := 0;
					centroidY := 0;
					oneHotIndex <= defaultValue;
					outputMask <= defaultValue;

					partialDist := 0;
					minDist := defaultDist;
					
					
                when idle =>
					if(i_start = '1') then
						fsmState <= start;
					end if;
            

				when start=>
				
					case startState is
					
						when idle=>
							startState <= readInMask;
						when readInMask=>
							inputMask := i_data;
							startState <= readX;
						when readX=>
							pointX := to_integer(unsigned(i_data));
							startState <= readY;
						when readY=>
							pointY := to_integer(unsigned(i_data));
							fsmState <= processing;
					end case;


				when processing=>
            
                    case executionState is

                        when idle=>
                            executionState <= checkMask;
                        when checkMask=>
                            if (centroidIndex = 8) then
                                fsmState <= ending;
                            else
                                -- if the corresponding bit of the inMask is '1'
                                if (inputMask(centroidIndex) = '1') then
                                    executionState <= getCentroidX;
                                else
                                    centroidIndex <= centroidIndex + 1;
                                end if;
                            end if;

                        when getCentroidX=>
                            centroidX := to_integer(unsigned(i_data));
                            executionState <= getCentroidY;

                        when getCentroidY=>
                            centroidY := to_integer(unsigned(i_data));
                            executionState <= evaluateDist;

                        when evaluateDist=>
                            partialDist := abs(pointX - centroidX) + abs(pointY - centroidY);
                            
                            if (partialDist > minDist) then
                                centroidIndex <= centroidIndex + 1;
                                executionState <= checkMask;
                            else
                                oneHotIndex <= std_logic_vector(one sll centroidIndex);

                                if (partialDist < minDist) then
                                    outputMask <= defaultValue;
                                    minDist := partialDist;
                                end if;

                                executionState <= modifyOutMask;

                            end if;

                        when modifyOutMask=>
                            -- OR between outputMask and oneHotIndex
                            outputMask <= outputMask or oneHotIndex;
                            centroidIndex <= centroidIndex + 1;
                            executionState <= checkMask;

                    end case;


				when ending=>
					case endState is
					
						when idle =>
							endState <= writeMask;

						when writeMask =>
							fsmState <= done;
					end case;


				when done=>
					fsmState <= reset;

			end case;
		end if;
	end process;

	-- Dataflow approach for output signals

	o_address  <= inMaskAddress   WHEN fsmState = start and startState = idle         ELSE
		          pointXAddress   WHEN fsmState = start and startState = readInMask   ELSE
		          pointYAddress   WHEN fsmState = start and startState = readX        ELSE
		          outMaskAddress  WHEN fsmState = ending                              ELSE
		          std_logic_vector(to_unsigned(((centroidIndex*2)+1),16)) WHEN fsmState = processing and executionState = checkMask     ELSE
                  std_logic_vector(to_unsigned(((centroidIndex*2)+2),16)) WHEN fsmState = processing and executionState = getCentroidX  ELSE
                  defaultAddress;
	o_done     <=        '1' WHEN fsmState = ending   and endState = writeMask ELSE '0';
    o_we       <=        '1' WHEN fsmState = ending   and endState = writeMask ELSE '0';
	o_en       <=        '1' WHEN fsmState = start     or fsmState = processing or fsmState = ending ELSE '0';
	o_data     <= outputMask WHEN fsmState = ending   and endState = writeMask ELSE defaultValue;

end Behavioral;
