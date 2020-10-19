with Interfaces;

--  Virtual Audio Processing Unit

package VirtAPU is

   type Channel_ID is new Positive;

   subtype Sample is Float;

   type Instance
     (Number_Of_Channels : Channel_ID;
      Sample_Rate        : Positive)
   is tagged limited
   private;

   procedure Tick (This : in out Instance);
   --  This procedure has to be called at a fixed frequency

   type Mode_Kind is (Pulse, Triangle, Noise_1, Noise_2);
   --  Mode of the channel oscillator

   type Tick_Count is new Interfaces.Unsigned_32;

   type Volume is range 0 .. 100;

   type Frequency is delta 0.0001 range 00.0 .. 45_000.0
     with Size => 32;

   type Sweep_Kind is (None, Up, Down);

   type Pulse_Width is range 10 .. 90;

   procedure Set_Mode (This    : in out Instance;
                       Chan_Id : Channel_ID;
                       Mode    : Mode_Kind)
     with Pre => Chan_Id <= This.Number_Of_Channels;
   --  Change the mode of the channel oscillator

   procedure Note_On (This    : in out Instance;
                      Chan_Id :        Channel_ID;
                      Freq    :        Frequency)
     with Pre => Chan_Id <= This.Number_Of_Channels;
   --  Start playing a note on the given channel

   procedure Note_Off (This    : in out Instance;
                       Chan_Id :        Channel_ID)
     with Pre => Chan_Id <= This.Number_Of_Channels;
   --  Stop playing a note on the given channel. If the decay is set to zero,
   --  the channel is muted after this call. If the decay is above zero, the
   --  volume will start to lower after this call.

   procedure Set_Width (This    : in out Instance;
                        Chan_Id :        Channel_ID;
                        Width   :        Pulse_Width)
     with Pre => Chan_Id <= This.Number_Of_Channels;
   --  Set pulse width for the given channel

   procedure Set_Sweep (This        : in out Instance;
                        Chan_Id     :        Channel_ID;
                        Kind        :        Sweep_Kind;
                        Sweep_Len   :        Positive;
                        Sweep_Ticks :        Tick_Count)
     with Pre => Chan_Id <= This.Number_Of_Channels;
   --  Set sweep parameters for the channel. Sweep will modulate the frequency
   --  of the oscillator. Sweep divides or multiplies the oscillator frequency
   --  every Sweep_Ticks.

   procedure Set_Volume (This    : in out Instance;
                         Chan_Id :        Channel_ID;
                         Vol     :        Volume)
     with Pre => Chan_Id <= This.Number_Of_Channels;
   --  Set the Volume for the given channel. When Note_On is called, the output
   --  level is set to this value.

   No_Decay  : constant Tick_Count := 0;

   procedure Set_Decay (This    : in out Instance;
                        Chan_Id :        Channel_ID;
                        Ticks   :        Tick_Count)
     with Pre => Chan_Id <= This.Number_Of_Channels;
   --  Set the number of ticks it will take for the channel envelope to go from
   --  the set volume to 0.

   -- Audio Output --

   function Next_Sample (This : in out Instance) return Sample;
   --  Return the next sample from all the channels mixed

   generic
      type Int_T is range <>;
      type Buffer_T is array (Natural range <>) of Int_T;
   procedure Next_Samples_Int (This   : in out Instance;
                               Buffer :    out Buffer_T);
   --  Return a buffer of samples from all the channels mixed and converted
   --  into a signed integer type.

   generic
      type UInt_T is mod <>;
      type Buffer_T is array (Natural range <>) of UInt_T;
   procedure Next_Samples_UInt (This   : in out Instance;
                                Buffer :    out Buffer_T);
   --  Return a buffer of samples from all the channels mixed and converted
   --  into a unsigned integer type.

   -- Sequencer --

   type Note_Duration is (Large, Long, Double, Whole, Half, Quarter, N_8th,
                          N_16th, N_32nd, N_64th, N_128th, N_256th);

   type Command_Kind is (Wait_Ticks, Wait_Note, Note_On, Note_Off, Set_Volume,
                         Set_Decay, Set_Sweep, Set_Width, Set_Mode);
   --  The command kinds correspond to the procedures above. With the addition
   --  of Wait_Ticks and Wait_Note to control timing.

   type Command (Kind : Command_Kind := Note_Off) is record
      case Kind is
         when Note_Off => null;
         when Note_On =>
            Freq : Frequency;
         when Wait_Ticks =>
            Ticks : Tick_Count;
         when Wait_Note =>
            Note : Note_Duration;
         when Set_Volume =>
            Vol : Volume;
         when Set_Decay =>
            Decay_Ticks : Tick_Count;
         when Set_Sweep =>
            Sweep       : Sweep_Kind;
            Sweep_Len   : Positive;
            Sweep_Ticks : Tick_Count;
         when Set_Width =>
            Width : Pulse_Width;
         when Set_Mode =>
            Mode : Mode_Kind;
      end case;
   end record;
   --  The Command record specifies the command kind and the associated
   --  parameters.

   type Command_Array is array (Natural range <>) of Command;
   type Sequence is not null access constant Command_Array;

   Empty : aliased constant Command_Array := (1 .. 0  => <>);
   Empty_Seq : Sequence := Empty'Access;

   procedure Set_Rhythm (This             : in out Instance;
                         BPM              :        Positive;
                         Ticks_Per_Second :        Positive);
   --  Set the rhythm for all channels. Ticks_Per_Second declares the frequency
   --  at which the Tick procedure will be called.

   procedure Set_Rhythm (This             : in out Instance;
                         Chan_Id          :        Channel_ID;
                         BPM              :        Positive;
                         Ticks_Per_Second :        Positive);
   --  Set the rhythm for one channel. Ticks_Per_Second declares the frequency
   --  at which the Tick procedure will be called.

   procedure Run (This    : in out Instance;
                  Chan_Id :        Channel_ID;
                  Seq     :        Sequence;
                  Looping :        Boolean := False);
   --  Start running a sequence on the given channel. If Looping is True the
   --  sequence will repeat until it is replaced by another sequence.

   --  Declaration of note duration commands for convenience
   C_Large        : constant Command := (Wait_Note, Large);
   C_Long         : constant Command := (Wait_Note, Long);
   C_Double       : constant Command := (Wait_Note, Double);
   C_Whole        : constant Command := (Wait_Note, Whole);
   C_Half         : constant Command := (Wait_Note, Half);
   C_Quarter      : constant Command := (Wait_Note, Quarter);
   C_8th          : constant Command := (Wait_Note, N_8th);
   C_16th         : constant Command := (Wait_Note, N_16th);
   C_32nd         : constant Command := (Wait_Note, N_32nd);
   C_64th         : constant Command := (Wait_Note, N_64th);
   C_128th        : constant Command := (Wait_Note, N_128th);
   C_256th        : constant Command := (Wait_Note, N_256th);

   Off : constant Command := (Kind => Note_Off);

   --  Declaration of note frequency commands for convenience
   pragma Warnings (Off, "*not a multiple of Small*");
   C0  : constant Command := (Note_On, 16.35);
   Cs0 : constant Command := (Note_On, 17.32);
   D0  : constant Command := (Note_On, 18.35);
   Ds0 : constant Command := (Note_On, 19.45);
   E0  : constant Command := (Note_On, 20.60);
   F0  : constant Command := (Note_On, 21.83);
   Fs0 : constant Command := (Note_On, 23.12);
   G0  : constant Command := (Note_On, 24.50);
   Gs0 : constant Command := (Note_On, 25.96);
   A0  : constant Command := (Note_On, 27.50);
   As0 : constant Command := (Note_On, 29.14);
   B0  : constant Command := (Note_On, 30.87);
   C1  : constant Command := (Note_On, 32.70);
   Cs1 : constant Command := (Note_On, 34.65);
   D1  : constant Command := (Note_On, 36.71);
   Ds1 : constant Command := (Note_On, 38.89);
   E1  : constant Command := (Note_On, 41.20);
   F1  : constant Command := (Note_On, 43.65);
   Fs1 : constant Command := (Note_On, 46.25);
   G1  : constant Command := (Note_On, 49.00);
   Gs1 : constant Command := (Note_On, 51.91);
   A1  : constant Command := (Note_On, 55.00);
   As1 : constant Command := (Note_On, 58.27);
   B1  : constant Command := (Note_On, 61.74);
   C2  : constant Command := (Note_On, 65.41);
   Cs2 : constant Command := (Note_On, 69.30);
   D2  : constant Command := (Note_On, 73.42);
   Ds2 : constant Command := (Note_On, 77.78);
   E2  : constant Command := (Note_On, 82.41);
   F2  : constant Command := (Note_On, 87.31);
   Fs2 : constant Command := (Note_On, 92.50);
   G2  : constant Command := (Note_On, 98.00);
   Gs2 : constant Command := (Note_On, 103.83);
   A2  : constant Command := (Note_On, 110.00);
   As2 : constant Command := (Note_On, 116.54);
   B2  : constant Command := (Note_On, 123.47);
   C3  : constant Command := (Note_On, 130.81);
   Cs3 : constant Command := (Note_On, 138.59);
   D3  : constant Command := (Note_On, 146.83);
   Ds3 : constant Command := (Note_On, 155.56);
   E3  : constant Command := (Note_On, 164.81);
   F3  : constant Command := (Note_On, 174.61);
   Fs3 : constant Command := (Note_On, 185.00);
   G3  : constant Command := (Note_On, 196.00);
   Gs3 : constant Command := (Note_On, 207.65);
   A3  : constant Command := (Note_On, 220.00);
   As3 : constant Command := (Note_On, 233.08);
   B3  : constant Command := (Note_On, 246.94);
   C4  : constant Command := (Note_On, 261.63);
   Cs4 : constant Command := (Note_On, 277.18);
   D4  : constant Command := (Note_On, 293.66);
   Ds4 : constant Command := (Note_On, 311.13);
   E4  : constant Command := (Note_On, 329.63);
   F4  : constant Command := (Note_On, 349.23);
   Fs4 : constant Command := (Note_On, 369.99);
   G4  : constant Command := (Note_On, 392.00);
   Gs4 : constant Command := (Note_On, 415.30);
   A4  : constant Command := (Note_On, 440.00);
   As4 : constant Command := (Note_On, 466.16);
   B4  : constant Command := (Note_On, 493.88);
   C5  : constant Command := (Note_On, 523.25);
   Cs5 : constant Command := (Note_On, 554.37);
   D5  : constant Command := (Note_On, 587.33);
   Ds5 : constant Command := (Note_On, 622.25);
   E5  : constant Command := (Note_On, 659.25);
   F5  : constant Command := (Note_On, 698.46);
   Fs5 : constant Command := (Note_On, 739.99);
   G5  : constant Command := (Note_On, 783.99);
   Gs5 : constant Command := (Note_On, 830.61);
   A5  : constant Command := (Note_On, 880.00);
   As5 : constant Command := (Note_On, 932.33);
   B5  : constant Command := (Note_On, 987.77);
   C6  : constant Command := (Note_On, 1046.50);
   Cs6 : constant Command := (Note_On, 1108.73);
   D6  : constant Command := (Note_On, 1174.66);
   Ds6 : constant Command := (Note_On, 1244.51);
   E6  : constant Command := (Note_On, 1318.51);
   F6  : constant Command := (Note_On, 1396.91);
   Fs6 : constant Command := (Note_On, 1479.98);
   G6  : constant Command := (Note_On, 1567.98);
   Gs6 : constant Command := (Note_On, 1661.22);
   A6  : constant Command := (Note_On, 1760.00);
   As6 : constant Command := (Note_On, 1864.66);
   B6  : constant Command := (Note_On, 1975.53);
   C7  : constant Command := (Note_On, 2093.00);
   Cs7 : constant Command := (Note_On, 2217.46);
   D7  : constant Command := (Note_On, 2349.32);
   Ds7 : constant Command := (Note_On, 2489.02);
   E7  : constant Command := (Note_On, 2637.02);
   F7  : constant Command := (Note_On, 2793.83);
   Fs7 : constant Command := (Note_On, 2959.96);
   G7  : constant Command := (Note_On, 3135.96);
   Gs7 : constant Command := (Note_On, 3322.44);
   A7  : constant Command := (Note_On, 3520.00);
   As7 : constant Command := (Note_On, 3729.31);
   B7  : constant Command := (Note_On, 3951.07);
   C8  : constant Command := (Note_On, 4186.01);
   Cs8 : constant Command := (Note_On, 4434.92);
   D8  : constant Command := (Note_On, 4698.63);
   Ds8 : constant Command := (Note_On, 4978.03);
   E8  : constant Command := (Note_On, 5274.04);
   F8  : constant Command := (Note_On, 5587.65);
   Fs8 : constant Command := (Note_On, 5919.91);
   G8  : constant Command := (Note_On, 6271.93);
   Gs8 : constant Command := (Note_On, 6644.88);
   A8  : constant Command := (Note_On, 7040.00);
   As8 : constant Command := (Note_On, 7458.62);
   B8  : constant Command := (Note_On, 7902.13);
   pragma Warnings (On, "*not a multiple of Small*");

private

   type Enveloppe_State is (Mute, Note_On, Decay);

   Ring_Buffer_Size : constant := 16;

   type Ring_Buffer_T is array (0 .. Ring_Buffer_Size - 1) of Sample;
   type BLIT_State is (Up, Down);

   type Channel is
      record
         Sweep                 : Sweep_Kind := None with Volatile;
         Sweep_Len             : Positive := 1 with Volatile;
         Sweep_Ticks           : Tick_Count := 0 with Volatile;
         Sweep_Remaining       : Natural with Volatile;
         Sweep_Remaining_Ticks : Tick_Count with Volatile;

         Vol             : Sample := 1.0 with Volatile;
         Env_State       : Enveloppe_State := Mute with Volatile;
         Decay_Ticks     : Tick_Count := No_Decay with Volatile;
         Decay_Remaining : Tick_Count := 0 with Volatile;

         Freq  : Frequency := 440.0 with Volatile;
         Mode  : Mode_Kind := Pulse with Volatile;
         Level : Sample    := 0.0 with Volatile;

         -- BLIT --
         Ring_Buffer            : Ring_Buffer_T;
         Next_Impulse_Time      : Natural := 0;
         Next_Impulse_Phase     : Float := 0.0;
         Trig_Rate              : Sample := 0.0;
         Remaining_BLIT_Samples : Natural := 0;
         Last_Sum               : Sample := 0.0;
         CSample_Nb             : Natural := 0;
         State                  : BLIT_State := Down;
         Width                  : Float := 0.1;
         LFSR                   : Interfaces.Unsigned_16 := 16#7FFF#;

         -- Sequencer --
         BPM                 : Positive;
         Ticks_Per_Second    : Positive;
         Seq                 : Sequence := Empty_Seq;
         Seq_Loop            : Boolean := False;
         Seq_Index           : Natural := 0;
         Seq_Remaining_Ticks : Tick_Count := 0;
      end record;

   type Channel_Array is array (Channel_ID range <>) of Channel;

   type Instance
     (Number_Of_Channels : Channel_ID;
      Sample_Rate        : Positive)
   is tagged limited
      record
         Channels : Channel_Array (1 .. Number_Of_Channels);

         Vol         : Volume;
         Decay_Vol   : Volume;
         Decay_Ticks : Tick_Count;

         Seq       : Sequence := Empty_Seq;
         Seq_Index : Natural  := Empty_Seq'First;
      end record;

end VirtAPU;
