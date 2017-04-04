//Copyright 2017 Andrey S. Ionisyan (anserion@gmail.com)
//
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button_exit: TButton;
    CheckGroupA: TCheckGroup;
    CheckGroup_apb_pow2: TCheckGroup;
    CheckGroup_apb_pow3: TCheckGroup;
    CheckGroup_C: TCheckGroup;
    CheckGroup_b7: TCheckGroup;
    CheckGroup_apb_pow3_minus_b7: TCheckGroup;
    CheckGroup_apb_pow3_minus_b7_plus_a: TCheckGroup;
    CheckGroup_D: TCheckGroup;
    CheckGroupB: TCheckGroup;
    CheckGroup_AplusB: TCheckGroup;
    Memo_help: TMemo;
    procedure Button_exitClick(Sender: TObject);
    procedure CheckGroupAItemClick(Sender: TObject; Index: LongInt);
    procedure CheckGroupBItemClick(Sender: TObject; Index: LongInt);
  private
    { private declarations }
  public
    { public declarations }
    procedure calc_formula;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
   tbit=(zero,one);
   tbit_vector=array of tbit;
   tbit_table=array of tbit_vector;

//=======================================================================
   function tt_not(op1:tbit):tbit;
   begin
        if op1=one then tt_not:=zero;
        if op1=zero then tt_not:=one;
   end;

   function tt_and(op1,op2:tbit):tbit;
   begin
        if (op1=zero)and(op2=zero) then tt_and:=zero;
        if (op1=zero)and(op2=one) then tt_and:=zero;
        if (op1=one)and(op2=zero) then tt_and:=zero;
        if (op1=one)and(op2=one) then tt_and:=one;
   end;

   function tt_xor(op1,op2:tbit):tbit;
   begin
        if (op1=zero)and(op2=zero) then tt_xor:=zero;
        if (op1=zero)and(op2=one) then tt_xor:=one;
        if (op1=one)and(op2=zero) then tt_xor:=one;
        if (op1=one)and(op2=one) then tt_xor:=zero;
   end;

   function tt_nand(op1,op2:tbit):tbit;
   begin
        if (op1=zero)and(op2=zero) then tt_nand:=one;
        if (op1=zero)and(op2=one) then tt_nand:=zero;
        if (op1=one)and(op2=zero) then tt_nand:=zero;
        if (op1=one)and(op2=one) then tt_nand:=zero;
   end;

   function tt_or(op1,op2:tbit):tbit;
   begin
        if (op1=zero)and(op2=zero) then tt_or:=zero;
        if (op1=zero)and(op2=one) then tt_or:=one;
        if (op1=one)and(op2=zero) then tt_or:=one;
        if (op1=one)and(op2=one) then tt_or:=one;
   end;

   //bits cutter
   function bin_cut_bits(start_pos,end_pos:integer; var x:tbit_vector):tbit_vector;
   var tmp:tbit_vector; i:integer;
   begin
      setlength(tmp,end_pos-start_pos+1);
      for i:=start_pos to end_pos do tmp[i-start_pos]:=x[i];
      bin_cut_bits:=tmp;
   end;

   //bits setter
   procedure bin_ins_bits(start_pos,end_pos:integer; var src,dst:tbit_vector);
   var i:integer;
   begin for i:=start_pos to end_pos do dst[i]:=src[i-start_pos]; end;

   procedure bin_set_bits(start_pos,end_pos:integer; value:tbit; var x:tbit_vector);
   var i:integer;
   begin for i:=start_pos to end_pos do x[i]:=value; end;

   {half-adder}
   procedure bin_half_adder(a,b:tbit; var s,c:tbit);
   begin
       c:=tt_and(a,b);
       s:=tt_xor(a,b);
   end;

   {full-adder}
   procedure bin_full_adder(a,b,c_in:tbit; var s,c_out:tbit);
   var s1,p1,p2:tbit;
   begin
       bin_half_adder(a,b,s1,p1);
       bin_half_adder(s1,c_in,s,p2);
       c_out:=tt_or(p1,p2);
   end;

   {n-bit adder}
   procedure bin_adder(a,b:tbit_vector; var s:tbit_vector);
   var i,n:integer; c:tbit_vector;
   begin
   n:=length(a); setlength(c,n+1);
   c[0]:=zero;
   for i:=0 to n-1 do bin_full_adder(a[i],b[i],c[i],s[i],c[i+1]);
   setlength(c,0);
   end;

   {n-bit subtractor}
   procedure bin_subtractor(a,b:tbit_vector; var s:tbit_vector);
   var i,n:integer; c:tbit_vector;
   begin
   n:=length(a); setlength(c,n+1);
   c[0]:=one;
   for i:=0 to n-1 do bin_full_adder(a[i],tt_not(b[i]),c[i],s[i],c[i+1]);
   setlength(c,0);
   end;

   {n-bit multiplier}
   procedure bin_multiplier(a,b:tbit_vector; var s:tbit_vector);
   var i,n:integer;
       tmp_sum,tmp_op1:tbit_table;
   begin
   n:=length(a);
   setlength(tmp_op1,n); for i:=0 to n-1 do setlength(tmp_op1[i],n);
   setlength(tmp_sum,n+1); for i:=0 to n do setlength(tmp_sum[i],n+1);
   for i:=0 to n-1 do tmp_sum[0,i]:=zero;
   for i:=0 to n-1 do
   begin
       if b[i]=one then
       begin
          bin_set_bits(0,i-1,zero,tmp_op1[i]);
          bin_ins_bits(i,n-1,a,tmp_op1[i]);
       end
       else bin_set_bits(0,n-1,zero,tmp_op1[i]);
       bin_adder(tmp_op1[i],tmp_sum[i],tmp_sum[i+1]);
   end;
   bin_ins_bits(0,n-1,tmp_sum[n],s);
   for i:=0 to n-1 do setlength(tmp_op1[i],0); setlength(tmp_op1,0);
   for i:=0 to n do setlength(tmp_sum[i],0); setlength(tmp_sum,0);
   end;

   {n-bit equal compare}
   procedure bin_is_equal(a,b:tbit_vector; var res:tbit);
   var res_tmp:tbit_vector; i,n:integer;
   begin
      n:=length(a); setlength(res_tmp,n+1);
      res_tmp[0]:=zero;
      for i:=0 to n-1 do res_tmp[i+1]:=tt_or(res_tmp[i],tt_xor(a[i],b[i]));
      res:=tt_not(res_tmp[n]);
      setlength(res_tmp,0);
   end;

   {n-bit greater compare. if a>b then res:=1}
   procedure bin_is_greater_than(a,b:tbit_vector; var res:tbit);
   var tmp_res,tmp_carry,tmp_cmp,tmp_equ:tbit_vector;
      i,n:integer;
   begin
      n:=length(a);
      setlength(tmp_res,n+1); setlength(tmp_carry,n+1);
      setlength(tmp_cmp,n); setlength(tmp_equ,n);

      tmp_res[n]:=zero;
      tmp_carry[n]:=one;
      for i:=n-1 downto 0 do
      begin
         tmp_cmp[i]:=tt_and(a[i],tt_not(b[i]));
         tmp_equ[i]:=tt_not(tt_xor(a[i],b[i]));
         tmp_carry[i]:=tt_and(tmp_carry[i+1],tmp_equ[i]);
         tmp_res[i]:=tt_or(tmp_res[i+1],tt_and(tmp_carry[i+1],tmp_cmp[i]));
      end;

      res:=tmp_res[0];
      setlength(tmp_res,0); setlength(tmp_carry,0);
      setlength(tmp_cmp,0); setlength(tmp_equ,0);
   end;

   {n-bit divider}
   procedure bin_divider(a,b:tbit_vector; var q,r:tbit_vector);
   var tmp_q,tmp_equal,tmp_greater: tbit_vector;
      tmp_r,tmp_b: tbit_table;
      i,n:integer;
   begin
   n:=length(a);
   setlength(tmp_q,n); setlength(tmp_equal,n); setlength(tmp_greater,n);
   setlength(tmp_r,n+1); setlength(tmp_b,n+1);
   for i:=0 to n do
   begin
      setlength(tmp_r[i],2*n-1);
      setlength(tmp_b[i],2*n-1);
   end;

   bin_set_bits(n,2*n-1,zero,tmp_r[0]);
   bin_ins_bits(0,n-1,a,tmp_r[0]);
   for i:=0 to n-1 do
   begin
     bin_is_greater_than(bin_cut_bits(n-i-1,n+n-i-2,tmp_r[i]),b,tmp_greater[n-i-1]);
     bin_is_equal(bin_cut_bits(n-i-1,n+n-i-2,tmp_r[i]),b,tmp_equal[n-i-1]);
     tmp_q[n-i-1]:=tt_or(tmp_greater[n-i-1],tmp_equal[n-i-1]);
     bin_set_bits(n+n-i-1,n+n-1,zero,tmp_b[i]);
     bin_set_bits(0,n-i-2,zero,tmp_b[i]);
     if tmp_q[n-i-1]=zero then bin_set_bits(n-i-1,n+n-i-2,zero,tmp_b[i])
                          else bin_ins_bits(n-i-1,n+n-i-2,b,tmp_b[i]);
     bin_subtractor(tmp_r[i],tmp_b[i],tmp_r[i+1]);
   end;

   q:=tmp_q;
   bin_ins_bits(0,n-1,tmp_r[n],r);
   setlength(tmp_q,0); setlength(tmp_equal,0); setlength(tmp_greater,0);
   for i:=0 to n do
   begin
      setlength(tmp_r[i],0);
      setlength(tmp_b[i],0);
   end;
   setlength(tmp_r,0); setlength(tmp_b,0);
   end;

   //======================================================================

{ TForm1 }

procedure TForm1.calc_formula;
var clk_num,sim_time,i,n:LongInt;
    GCLK:tbit;
   a,b,c,d:tbit_vector;
   a_plus_b,apb_pow2,apb_pow3,b7,apb_pow3_minus_b7,
   apb_pow3_minus_b7_plus_a,seven:tbit_vector;
begin
//input data tuning
n:=10; setlength(a,n); setlength(b,n); setlength(c,n+1); setlength(d,n+1);

//intermediate variables tuning
setlength(a_plus_b,n);
setlength(apb_pow2,n);
setlength(apb_pow3,n);
setlength(b7,n);
setlength(apb_pow3_minus_b7,n);
setlength(apb_pow3_minus_b7_plus_a,n);
setlength(seven,n);
seven[0]:=one; seven[1]:=one; seven[2]:=one;
for i:=3 to n-1 do seven[i]:=zero;

//let's go
GCLK:=zero;  clk_num:=0; sim_time:=2;

while clk_num<sim_time do
begin
//get input data
for i:=0 to n-1 do
begin
     if CheckGroupA.Checked[i] then a[i]:=one else a[i]:=zero;
     if CheckGroupB.Checked[i] then b[i]:=one else b[i]:=zero;
end;

//-----------------------------------------
//test formula:
// c = ((a+b)^3 - 7*b + a) div b,
// d = ((a+b)^3 - 7*b + a) mod b
//-----------------------------------------
//1) a_plus_b=a+b
bin_adder(a,b,a_plus_b);
//report
for i:=0 to n-1 do
    if a_plus_b[i]=one then CheckGroup_AplusB.Checked[i]:=true
                       else CheckGroup_AplusB.Checked[i]:=false;
//2) apb_pow2=a_plus_b*a_plus_b
bin_multiplier(a_plus_b,a_plus_b,apb_pow2);
//report
for i:=0 to n-1 do
    if apb_pow2[i]=one then CheckGroup_apb_pow2.Checked[i]:=true
                       else CheckGroup_apb_pow2.Checked[i]:=false;
//3) apb_pow3=apb_pow2*a_plus_b
bin_multiplier(apb_pow2,a_plus_b,apb_pow3);
//report
for i:=0 to n-1 do
    if apb_pow3[i]=one then CheckGroup_apb_pow3.Checked[i]:=true
                       else CheckGroup_apb_pow3.Checked[i]:=false;
//4) b7=b*7
bin_multiplier(b,seven,b7);
//report
for i:=0 to n-1 do
    if b7[i]=one then CheckGroup_b7.Checked[i]:=true
                 else CheckGroup_b7.Checked[i]:=false;
//5) apb_pow3_minus_b7=apb_pow3-b7
bin_subtractor(apb_pow3,b7,apb_pow3_minus_b7);
//report
for i:=0 to n-1 do
    if apb_pow3_minus_b7[i]=one then CheckGroup_apb_pow3_minus_b7.Checked[i]:=true
                                else CheckGroup_apb_pow3_minus_b7.Checked[i]:=false;
//6) apb_pow3_minus_b7_plus_a=apb_pow3_minus_b7+a
bin_adder(apb_pow3_minus_b7,a,apb_pow3_minus_b7_plus_a);
//report
for i:=0 to n-1 do
    if apb_pow3_minus_b7_plus_a[i]=one then CheckGroup_apb_pow3_minus_b7_plus_a.Checked[i]:=true
                                       else CheckGroup_apb_pow3_minus_b7_plus_a.Checked[i]:=false;
//7) c=apb_pow3_minus_b7_plus_a div b; d=apb_pow3_minus_b7_plus_a mod b
bin_divider(apb_pow3_minus_b7_plus_a,b,c,d);
//report
for i:=0 to n-1 do
begin
    if c[i]=one then CheckGroup_c.Checked[i]:=true else CheckGroup_c.Checked[i]:=false;
    if d[i]=one then CheckGroup_d.Checked[i]:=true else CheckGroup_d.Checked[i]:=false;
end;

GCLK:=tt_not(GCLK); clk_num:=clk_num+1;
end;

//clear memory
setlength(a,0);
setlength(b,0);
setlength(c,0);
setlength(d,0);
setlength(a_plus_b,0);
setlength(apb_pow2,0);
setlength(apb_pow3,0);
setlength(b7,0);
setlength(apb_pow3_minus_b7,0);
setlength(apb_pow3_minus_b7_plus_a,0);
setlength(seven,0);
end;

procedure TForm1.Button_exitClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.CheckGroupAItemClick(Sender: TObject; Index: LongInt);
begin
  calc_formula;
end;

procedure TForm1.CheckGroupBItemClick(Sender: TObject; Index: LongInt);
begin
  calc_formula;
end;

end.

