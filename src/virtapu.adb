with Interfaces; use Interfaces;

package body VirtAPU is

   generic
      type Int_T is range <>;
   function To_Int (S : Sample) return Int_T
     with Inline;

   generic
      type UInt_T is mod <>;
   function To_UInt (S : Sample) return UInt_T
     with Inline;

   function Saturate (S : Sample) return Sample
     with Inline;

   function Next_Sample_Pulse_And_Noise
     (Chan        : in out Channel;
      Sample_Rate : Positive)
      return Sample
     with Inline;

   function Next_Sample_Triangle
     (Chan : in out Channel;
      Sample_Rate : Positive)
      return Sample
     with Inline;

   procedure Process_Seq (This    : in out Instance;
                          Chan_Id :        Channel_ID);

   -----------------------
   -- Period_In_Samples --
   -----------------------

   function Period_In_Samples (Sample_Rate : Positive;
                               Rate_In_Hertz : Frequency)
                               return Float
   is (Float (Sample_Rate) / Float (Rate_In_Hertz))
     with Inline;

   ---------------------------------
   -- Next_Sample_Pulse_And_Noise --
   ---------------------------------

   function Next_Sample_Pulse_And_Noise
     (Chan        : in out Channel;
      Sample_Rate : Positive)
      return Sample
   is
      Delta_Time    : Float;
      Width         : Float;
      Next_State    : BLIT_State := Down;
   begin
      Chan.CSample_Nb := Chan.CSample_Nb + 1;

      --  If it is time, compute the next BLIT step
      if Chan.Next_Impulse_Time <= Chan.CSample_Nb then

         if Chan.State = Up then
            Width := Chan.Width;
         else
            Width := 1.0 - Chan.Width;
         end if;

         Delta_Time := Period_In_Samples (Sample_Rate, Chan.Freq) * Width +
           Chan.Next_Impulse_Phase;

         Chan.Next_Impulse_Time := Chan.Next_Impulse_Time +
           Natural (Float'Floor (Delta_Time));

         Chan.Next_Impulse_Phase := Delta_Time - Float'Floor (Delta_Time);

         case Chan.Mode is
            when Pulse =>

               --  Invert the state
               Next_State := (if Chan.State = Up then Down else Up);

            when Noise_1 | Noise_2 =>

               --  Next state depends on LFSR noise
               declare
                  B0 : constant Unsigned_16 := Chan.LFSR and 1;
                  B1 : constant Unsigned_16 := Shift_Right (Chan.LFSR, 1) and 1;
                  B6 : constant Unsigned_16 := Shift_Right (Chan.LFSR, 6) and 1;
                  Feedback : Unsigned_16;
               begin
                  if Chan.Mode = Noise_1 then
                     Feedback := B0 xor B1;
                  else
                     Feedback := B0 xor B6;
                  end if;

                  Next_State := (if B0 = 0 then Down else Up);

                  Chan.LFSR := Shift_Right (Chan.LFSR, 1);

                  if (Feedback and 1) /= 0 then
                     Chan.LFSR := Chan.LFSR or 16#4000#;
                  else
                     Chan.LFSR := Chan.LFSR and not 16#4000#;
                  end if;
               end;

            when others =>
               Next_State := Down;
         end case;

         if Next_State /= Chan.State then
            if Next_State = Up then
               Chan.State := Up;
            else
               Chan.State := Down;
            end if;
         end if;
      end if;

      if Chan.State = Up then
         return Chan.Level;
      else
         return -Chan.Level;
      end if;
   end Next_Sample_Pulse_And_Noise;

   --------------------------
   -- Next_Sample_Triangle --
   --------------------------

   function Next_Sample_Triangle
     (Chan : in out Channel;
      Sample_Rate : Positive)
      return Sample
   is
      Delta_Time    : Float;
      Width         : Float;
   begin
      Chan.CSample_Nb := Chan.CSample_Nb + 1;

      --  If it is time, compute the next change of direction
      if Chan.Next_Impulse_Time <= Chan.CSample_Nb then

         if Chan.State = Up then
            Width := Chan.Width;
         else
            Width := 1.0 - Chan.Width;
         end if;

         Delta_Time := Period_In_Samples (Sample_Rate, Chan.Freq) * Width +
           Chan.Next_Impulse_Phase;

         Chan.Next_Impulse_Time := Chan.Next_Impulse_Time +
           Natural (Float'Floor (Delta_Time));

         Chan.Next_Impulse_Phase := Delta_Time - Float'Floor (Delta_Time);

         if Chan.State = Up then
            Chan.State := Down;
            Chan.Last_Sum := 1.0;
            Chan.Trig_Rate := Sample (-2.0 / Delta_Time);
         else
            Chan.State := Up;
            Chan.Last_Sum := -1.0;
            Chan.Trig_Rate := Sample (2.0 / Delta_Time);
         end if;
      end if;

      --  Do the trig! /\/\/\
      Chan.Last_Sum := Chan.Last_Sum + Chan.Trig_Rate;
      return Chan.Last_Sum * Chan.Level;
   end Next_Sample_Triangle;

   --------------
   -- Saturate --
   --------------

   function Saturate (S : Sample) return Sample
   is (if S > 1.0 then 1.0
       elsif S < -1.0 then -1.0
       else S);

   ------------
   -- To_Int --
   ------------

   function To_Int (S : Sample) return Int_T is
   begin
      return Int_T (Float (Saturate (S)) * Float (Int_T'Last - 1));
   end To_Int;

   -------------
   -- To_UInt --
   -------------

   function To_UInt (S : Sample) return UInt_T is
   begin
      return UInt_T
        ((((Float (Saturate (S)) / 4.0) + 1.0) / 2.0)
         * Float (UInt_T'Last - 1));
   end To_UInt;

   -----------------
   -- Next_Sample --
   -----------------

   function Next_Sample (This : in out Instance) return Sample
   is
      S : Sample := 0.0;
   begin

      --  Mix the channels
      for Chan of This.Channels loop
         case Chan.Mode is
            when Pulse | Noise_1 | Noise_2 =>
               S := S + Next_Sample_Pulse_And_Noise (Chan, This.Sample_Rate);
            when Triangle =>
               S := S + Next_Sample_Triangle (Chan, This.Sample_Rate);
         end case;
      end loop;

      return S;
   end Next_Sample;

   ----------------------
   -- Next_Samples_Int --
   ----------------------

   procedure Next_Samples_Int (This   : in out Instance;
                               Buffer :    out Buffer_T)
   is
      function From_Sample is new To_Int (Int_T);
   begin
      for Elt of Buffer loop
         Elt := From_Sample (This.Next_Sample);
      end loop;
   end Next_Samples_Int;

   -----------------------
   -- Next_Samples_UInt --
   -----------------------

   procedure Next_Samples_UInt (This   : in out Instance;
                                Buffer :    out Buffer_T)
   is
      function From_Sample is new To_UInt (UInt_T);
   begin
      for Elt of Buffer loop
         Elt := From_Sample (This.Next_Sample);
      end loop;
   end Next_Samples_UInt;

   -----------------
   -- Process_Seq --
   -----------------

   procedure Process_Seq (This    : in out Instance;
                          Chan_Id :        Channel_ID)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      if Chan.Seq_Remaining_Ticks /= 0 then
         Chan.Seq_Remaining_Ticks := Chan.Seq_Remaining_Ticks - 1;
      end if;

      if Chan.Seq_Remaining_Ticks = 0 then
         while Chan.Seq_Index in Chan.Seq'Range loop
            declare
               Cmd : Command renames Chan.Seq (Chan.Seq_Index);
            begin
               case Cmd.Kind is

               when Wait_Ticks =>
                  Chan.Seq_Remaining_Ticks := Cmd.Ticks;

               when Wait_Note =>
                  Chan.Seq_Remaining_Ticks :=
                    Tick_Count ((60.0 / Float (Chan.BPM)) *
                                  Float (Chan.Ticks_Per_Second) *
                                (case Cmd.Note is
                                        when Large   => 8.0,
                                        when Long    => 4.0,
                                        when Double  => 2.0,
                                        when Whole   => 1.0,
                                        when Half    => 0.5,
                                        when Quarter => 0.25,
                                        when N_8th   => 0.125,
                                        when N_16th  => 0.0625,
                                        when N_32nd  => 0.0312,
                                        when N_64th  => 0.015625,
                                        when N_128th => 0.0078125,
                                        when N_256th => 0.0039062));

               when Note_On =>
                  Note_On (This, Chan_Id, Cmd.Freq);

               when Note_Off =>
                  Note_Off (This, Chan_Id);

               when Set_Decay =>
                  Set_Decay (This, Chan_Id, Cmd.Decay_Ticks);

               when Set_Sweep =>
                  Set_Sweep (This, Chan_Id,
                             Cmd.Sweep,
                             Cmd.Sweep_Len,
                             Cmd.Sweep_Ticks);

               when Set_Volume =>
                  Set_Volume (This, Chan_Id, Cmd.Vol);

               when Set_Mode =>
                  Set_Mode (This, Chan_Id, Cmd.Mode);

               when Set_Width =>
                  Set_Width (This, Chan_Id, Cmd.Width);

               end case;
            end;

            Chan.Seq_Index := Chan.Seq_Index + 1;

            if Chan.Seq_Index not in Chan.Seq'Range
              and then
                Chan.Seq_Loop
            then
               Chan.Seq_Index := Chan.Seq'First;
            end if;

            --  Exit if we have to wait before the next command
            exit when Chan.Seq_Remaining_Ticks /= 0;
         end loop;
      end if;
   end Process_Seq;

   ----------
   -- Tick --
   ----------

   procedure Tick (This : in out Instance) is
   begin
      --  Sequencer
      for Chan_Id in This.Channels'Range loop
         Process_Seq (This, Chan_Id);
      end loop;

      for Chan of This.Channels loop
         --  Sweep
         case Chan.Sweep is
            when None => null;
            when Up | Down =>
               if Chan.Sweep_Remaining_Ticks > 0 then
                  Chan.Sweep_Remaining_Ticks := Chan.Sweep_Remaining_Ticks - 1;
               end if;

               if Chan.Sweep_Remaining_Ticks = 0
                 and then
                  Chan.Sweep_Remaining > 0
               then
                  declare
                     Freq : constant Float := Float (Chan.Freq);
                     Sign : constant Float := (if Chan.Sweep = Up then
                                                  1.0
                                               else
                                                  -1.0);
                     Delt : constant Float :=
                       (Sign * Freq) / Float (2 ** Chan.Sweep_Remaining);
                  begin

                     Chan.Freq := Frequency (Freq + Delt);

                     Chan.Sweep_Remaining := Chan.Sweep_Remaining - 1;
                     Chan.Sweep_Remaining_Ticks := Chan.Sweep_Ticks;

                  end;
               end if;
         end case;

         --  Envelope
         case Chan.Env_State is
            when Decay =>
               if Chan.Decay_Remaining = 0 then
                  Chan.Env_State := Mute;
                  Chan.Level := 0.0;
               else
                  Chan.Decay_Remaining := Chan.Decay_Remaining - 1;
                  Chan.Level :=
                    Sample ((Float (Chan.Vol) / Float (Chan.Decay_Ticks)) *
                              Float (Chan.Decay_Remaining));
               end if;
            when others => null;
         end case;
      end loop;
   end Tick;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (This    : in out Instance;
                       Chan_Id : Channel_ID;
                       Mode    : Mode_Kind)
   is

      Chan : Channel renames This.Channels (Chan_Id);

   begin
      Chan.Mode := Mode;
      Chan.Last_Sum := 0.0;
      Chan.State := Down;
      Chan.CSample_Nb := 0;
      Chan.Next_Impulse_Phase := 0.0;
      Chan.Next_Impulse_Time := 0;
   end Set_Mode;

   -------------
   -- Note_On --
   -------------

   procedure Note_On (This    : in out Instance;
                      Chan_Id :        Channel_ID;
                      Freq    :        Frequency)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      Chan.Freq := Freq;
      Chan.Level := Chan.Vol;

      Chan.Env_State := Note_On;

      if Chan.Sweep /= None then
         Chan.Sweep_Remaining := Chan.Sweep_Len;
         Chan.Sweep_Remaining_Ticks := Chan.Sweep_Ticks;
      end if;
   end Note_On;

   --------------
   -- Note_Off --
   --------------

   procedure Note_Off (This    : in out Instance;
                       Chan_Id :        Channel_ID)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      if Chan.Decay_Ticks /= No_Decay then
         Chan.Env_State := Decay;
         Chan.Decay_Remaining := Chan.Decay_Ticks;
      else
         Chan.Env_State := Mute;
         Chan.Level := 0.0;
      end if;
   end Note_Off;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width (This    : in out Instance;
                        Chan_Id :        Channel_ID;
                        Width   :        Pulse_Width)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      Chan.Width := Float (Width) / 100.0;
   end Set_Width;

   ---------------
   -- Set_Sweep --
   ---------------

   procedure Set_Sweep (This        : in out Instance;
                        Chan_Id     :        Channel_ID;
                        Kind        :        Sweep_Kind;
                        Sweep_Len   :        Positive;
                        Sweep_Ticks :        Tick_Count)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      Chan.Sweep := Kind;
      Chan.Sweep_Len := Sweep_Len;
      Chan.Sweep_Ticks := Sweep_Ticks;
   end Set_Sweep;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume (This    : in out Instance;
                         Chan_Id :        Channel_ID;
                         Vol     :        Volume)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      Chan.Vol := Sample (Vol) / 100.0;
   end Set_Volume;

   ---------------
   -- Set_Decay --
   ---------------

   procedure Set_Decay (This    : in out Instance;
                        Chan_Id :        Channel_ID;
                        Ticks   :        Tick_Count)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      Chan.Decay_Ticks := Ticks;
   end Set_Decay;

   ----------------
   -- Set_Rhythm --
   ----------------

   procedure Set_Rhythm (This             : in out Instance;
                         BPM              :        Positive;
                         Ticks_Per_Second :        Positive)
   is
   begin
      for Chan of This.Channels loop
         Chan.BPM := BPM;
         Chan.Ticks_Per_Second := Ticks_Per_Second;
      end loop;
   end Set_Rhythm;

   ----------------
   -- Set_Rhythm --
   ----------------

   procedure Set_Rhythm (This             : in out Instance;
                         Chan_Id          :        Channel_ID;
                         BPM              :        Positive;
                         Ticks_Per_Second :        Positive)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      Chan.BPM := BPM;
      Chan.Ticks_Per_Second := Ticks_Per_Second;
   end Set_Rhythm;

   ---------
   -- Run --
   ---------

   procedure Run (This    : in out Instance;
                  Chan_Id :        Channel_ID;
                  Seq     :        Sequence;
                  Looping :        Boolean := False)
   is
      Chan : Channel renames This.Channels (Chan_Id);
   begin
      Chan.Seq := Seq;
      Chan.Seq_Index := Seq'First;
      Chan.Seq_Remaining_Ticks := 0;
      Chan.Seq_Loop := Looping;
   end Run;

end VirtAPU;
