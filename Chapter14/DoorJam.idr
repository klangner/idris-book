module DoorJam


data DoorState = DoorClosed | DoorOpen

data DoorResult = Ok | Jammed

data DoorCmd : (ty : Type) -> DoorState -> (ty -> DoorState) -> Type where
  Open : DoorCmd DoorResult DoorClosed (\res => case res of
                                                    Ok => DoorOpen
                                                    Jammed => DoorClosed)
  Close : DoorCmd () DoorOpen (const DoorClosed)
  RingBell : DoorCmd () DoorClosed (const DoorClosed)

  Pure : ty -> DoorCmd ty state (const state)
  (>>=) : DoorCmd a state1 state2_fn ->
          ((res : a) -> DoorCmd b (state2_fn res) state3_fn) ->
          DoorCmd b state1 state3_fn

doorProg : DoorCmd () DoorClosed (const DoorClosed)
doorProg = do RingBell
              Ok <- Open | Jammed => Pure ()
              Close
