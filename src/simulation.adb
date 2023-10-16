with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

-- Jerzy Szyjut 193064
-- Artur Bińczyk 193138

procedure Simulation is
  Number_Of_Products  : constant Integer := 9;
  Number_Of_Computers : constant Integer := 3;
  Number_Of_Clients   : constant Integer := 2;

  subtype Product_Type is Integer range 1 .. Number_Of_Products;
  subtype Computer_Type is Integer range 1 .. Number_Of_Computers;
  subtype Client_Type is Integer range 1 .. Number_Of_Clients;

  Product_Name  : constant array (Product_Type) of String (1 .. 11) :=
   ("CPU        ", "GPU        ", "RAMChip    ", "PowerSupply", "Cooling    ",
    "Motherboard", "HDD        ", "SSD        ", "Case       ");
  Computer_Name : constant array (Computer_Type) of String (1 .. 9) :=
   ("Computer1", "Computer2", "Computer3");

  package Random_Computer is new Ada.Numerics.Discrete_Random (Computer_Type);
  type My_Str is new String (1 .. 256);

  -- Producer produces determined product
  task type Producer is
    -- Give the Producer an identity, i.e. the product type
    entry Start (Product : in Product_Type; Production_Time : in Integer);
  end Producer;

  -- Client gets an arbitrary computer of several products from the shop
  task type Client is
    -- Give the Client an identity
    entry Start
     (Client_Number : in Client_Type; Consumption_Time : in Integer);
  end Client;

  -- In the Shop, products are assemblied into an computer
  task type Shop is
    -- Accept a product to the storage provided there is a room for it
    entry Take (Product : in Product_Type; Number : in Integer);
    -- Deliver an computer provided there are enough products for it
    entry Deliver (Computer : in Computer_Type; Number : out Integer);
  end Shop;

  P : array (1 .. Number_Of_Products) of Producer;
  C : array (1 .. Number_Of_Clients) of Client;
  S : Shop;

  task body Producer is
    subtype Production_Time_Range is Integer range 3 .. 6;
    package Random_Production is new Ada.Numerics.Discrete_Random
     (Production_Time_Range);
    G : Random_Production.Generator;   --  random number generator
    Product_Type_Number : Integer;
    Product_Number      : Integer;
    Production          : Integer;
  begin
    accept Start (Product : in Product_Type; Production_Time : in Integer) do
      Random_Production.Reset (G); --  start random number generator
      Product_Number      := 1;
      Product_Type_Number := Product;
      Production          := Production_Time;
    end Start;

    Put_Line ("Started producer of " & Product_Name (Product_Type_Number));
    loop
      delay Duration (Random_Production.Random (G)); --  simulate production

      Put_Line
       ("Produced product " & Product_Name (Product_Type_Number) & " number " &
        Integer'Image (Product_Number));

      -- Accept for storage
      S.Take (Product_Type_Number, Product_Number);
      Product_Number := Product_Number + 1;
    end loop;
  end Producer;

  task body Client is
    subtype Client_Cooldown is Integer range 3 .. 9;
    package Random_Cooldown is new Ada.Numerics.Discrete_Random
     (Client_Cooldown);

    G : Random_Cooldown.Generator;  --  random number generator (time)
    G2              : Random_Computer.Generator;    --  also (Computers)
    Client_Nb       : Client_Type;
    Computer_Number : Integer;
    Consumption     : Integer;
    Computer_Type   : Integer;
    Client_Name : constant array (1 .. Number_Of_Clients) of String (1 .. 7) :=
     ("Client1", "Client2");
  begin
    accept Start
     (Client_Number : in Client_Type; Consumption_Time : in Integer)
    do
      Random_Cooldown.Reset (G);   --  setting up generator
      Random_Computer.Reset (G2);
      Client_Nb   := Client_Number;
      Consumption := Consumption_Time;
    end Start;
    Put_Line ("Started shooping " & Client_Name (Client_Nb));
    loop
      delay Duration (Random_Cooldown.Random (G)); --  simulate shopping
      Computer_Type := Random_Computer.Random (G2);
      -- take an computer
      select
        S.Deliver (Computer_Type, Computer_Number);
      or --  if there is no computer, wait for one
        delay Duration (2);
        Put_Line
         ("Shop is busy " & Client_Name (Client_Nb) &
          " is waiting for a computer " & Computer_Name (Computer_Type));
      end select;
      if Computer_Number /= 0 then
        Put_Line
         (Client_Name (Client_Nb) & ": taken computer " &
          Computer_Name (Computer_Type) & " number " &
          Integer'Image (Computer_Number));
      else
        Put_Line
         (Client_Name (Client_Nb) & " failed to take a computer " &
          Computer_Name (Computer_Type));
        --that's why there were "taken computer number zero
        --we need to do something about it
      end if;
    end loop;
  end Client;

  task body Shop is
    Storage_Capacity : constant Integer := 40;
    type Storage_type is array (Product_Type) of Integer;
    --  Storage              : Storage_type := (0, 0, 0, 0, 0, 0, 0, 0, 0);
    Storage              : Storage_type := (1, 1, 4, 1, 1, 1, 3, 1, 1);
    Computer_Content     : array (Computer_Type, Product_Type) of Integer :=
     ((1, 1, 4, 1, 1, 1, 3, 1, 1), (1, 1, 2, 1, 1, 1, 0, 1, 1),
      (1, 0, 1, 1, 1, 1, 4, 2, 0));
    Max_Computer_Content : array (Product_Type) of Integer;
    Computer_Number      : array (Computer_Type) of Integer := (1, 1, 1);
    In_Storage           : Integer                                        := 0;

    procedure Setup_Variables is
    begin
      for W in Product_Type loop
        Max_Computer_Content (W) := 0;
        for Z in Computer_Type loop
          if Computer_Content (Z, W) > Max_Computer_Content (W) then
            Max_Computer_Content (W) := Computer_Content (Z, W);
          end if;
        end loop;
      end loop;
    end Setup_Variables;

    function Can_Accept (Product : Product_Type) return Boolean is
      Free          : Integer;         --  free room in the storage
      -- how many products are for production of arbitrary computer
      Lacking       : array (Product_Type) of Integer;
      -- how much room is needed in storage to produce arbitrary computer
      Lacking_Space : Integer;
      MP            : Boolean;                   --  can accept
    begin
      if In_Storage >= Storage_Capacity then
        return False;
      end if;

      -- There is free room in the storage
      Free := Storage_Capacity - In_Storage;
      MP   := True;

      for W in Product_Type loop
        if Storage (W) < Max_Computer_Content (W) then
          MP := False;
        end if;
      end loop;

      if MP then
        return
         True;                --  storage has products for arbitrary computer
      end if;
      if Integer'Max (0, Max_Computer_Content (Product) - Storage (Product)) >
       0
      then
        -- exactly this product lacks
        return True;
      end if;

      Lacking_Space := 1;                     --  insert current product
      for W in Product_Type loop
        Lacking (W) := Integer'Max (0, Max_Computer_Content (W) - Storage (W));
        Lacking_Space := Lacking_Space + Lacking (W);
      end loop;
      if Free >= Lacking_Space then
        -- there is enough room in storage for arbitrary product
        return True;
      else
        -- no room for this product
        return False;
      end if;
    end Can_Accept;

    function Can_Deliver (Computer : Computer_Type) return Boolean is
    begin
      for W in Product_Type loop
        if Storage (W) < Computer_Content (Computer, W) then
          return False;
        end if;
      end loop;
      return True;
    end Can_Deliver;

    procedure Storage_Contents is
    begin
      Put ("Storage contents: ");
      for W in Product_Type loop
        Put (Product_Name (W) & " " & Integer'Image (Storage (W)) & " ");
      end loop;
      New_Line;
    end Storage_Contents;

  begin
    Put_Line ("Shop started");
    Setup_Variables;
    loop
      accept Take (Product : in Product_Type; Number : in Integer) do
        if Can_Accept (Product) then
          Put_Line
           ("Accepted product " & Product_Name (Product) & " number " &
            Integer'Image (Number));

          Storage (Product) := Storage (Product) + 1;
          In_Storage        := In_Storage + 1;
        else
          Put_Line
           ("Rejected product " & Product_Name (Product) & " number " &
            Integer'Image (Number));
        end if;
      end Take;
      Storage_Contents;
      accept Deliver (Computer : in Computer_Type; Number : out Integer) do
        if Can_Deliver (Computer) then
          Put_Line
           ("Received order for " & Computer_Name (Computer) &
            " begining to assebmle");
          delay Duration (3); --  simulate delivery
          Put_Line
           ("Delivered computer " & Computer_Name (Computer) & " number " &
            Integer'Image (Computer_Number (Computer)));

          for W in Product_Type loop
            Storage (W) := Storage (W) - Computer_Content (Computer, W);
            In_Storage  := In_Storage - Computer_Content (Computer, W);
          end loop;
          Number                     := Computer_Number (Computer);
          Computer_Number (Computer) := Computer_Number (Computer) + 1;
        else
          Put_Line
           ("Lacking products for Computer " & Computer_Name (Computer));

          Number := 0;
        end if;
      end Deliver;
      Storage_Contents;
    end loop;
  end Shop;

begin
  for I in 1 .. Number_Of_Products loop
    P (I).Start (I, 10);
  end loop;
  for J in 1 .. Number_Of_Clients loop
    C (J).Start (J, 12);
  end loop;
end Simulation;
