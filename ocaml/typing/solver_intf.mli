type allowed = private Allowed

type disallowed = private Disallowed

type left_only = allowed * disallowed

type right_only = disallowed * allowed

type both = allowed * allowed

module type Allow_disallow = sig
  type ('a, 'b, 'd) t

  (** Disallows on the right.  *)
  val disallow_right : ('a, 'b, 'l * 'r) t -> ('a, 'b, 'l * disallowed) t

  (** Disallows a the left.  *)
  val disallow_left : ('a, 'b, 'l * 'r) t -> ('a, 'b, disallowed * 'r) t

  (** Generalizes a right-hand-side [allowed] to be any allowance.  *)
  val allow_right : ('a, 'b, 'l * allowed) t -> ('a, 'b, 'l * 'r) t

  (** Generalizes a left-hand-side [allowed] to be any allowance.  *)
  val allow_left : ('a, 'b, allowed * 'r) t -> ('a, 'b, 'l * 'r) t
end

(** A collection of lattices, indexed by [obj] *)
module type Lattices = sig
  (** Lattice identifers, indexed by ['a] the carrier type of that lattice *)
  type 'a obj

  val min : 'a obj -> 'a

  val max : 'a obj -> 'a

  val le : 'a obj -> 'a -> 'a -> bool

  val join : 'a obj -> 'a -> 'a -> 'a

  val meet : 'a obj -> 'a -> 'a -> 'a

  val print : 'a obj -> Format.formatter -> 'a -> unit

  val eq_obj : 'a obj -> 'b obj -> ('a, 'b) Misc.eq option
end

(** Extend [Lattices] with monotone functions (including identity) to form a
   category. Among those monotone functions some will have left and right
   adjoints. *)
module type Lattices_mono = sig
  include Lattices

  (** Morphism from object of base type ['a] to object of base type ['b].
      ['d] is ['l] * ['r], where ['l] can be:
      - [allowed], meaning the morphism can be on the left because it has right
        adjoint.
      - [disallowed], meaning the morphism cannot be on the left because
        it does not have right adjoint.
      Similar for ['r]. *)
  type ('a, 'b, 'd) morph

  (* Due to the implementation in [solver.ml], a mode doesn't have sufficient
     information to infer the object it lives in,  whether at compile-time or
     runtime. There is info at compile-time to distinguish between different
     carrier types, but one can imagine multiple objects with the same carrier
     type. Therefore, we can treat modes as object-blind.

     As a result, user of the solver needs to provide the object the modes live
     in, every time it invokes the solver on some modes.

     Roughly, ['a mode] is represented in the solver as constant of ['a], or [f
     v] where [f] is a morphism from ['b] to ['a] and [v] is some variable of
     ['b]. The ['a] needs additional ['a obj] to decide its position in the
     lattice structure (because again, multiple lattices can share the same
     carrier type). One might think the morphism [f] should know its own source
     and target objects. But since its target object is already given by the
     user for each invocation anyway, we decide to exploit this, and say that "a
     morphism is determined by some [('a, 'b, 'd) morph] together with some ['b
     obj]". That helps reduce the information each [morph] needs to store.

     As a result, in the interaction between the solver and the lattices,
     [morph] always comes with its target object. *)

  (** Give the source object of a morphism  *)
  val src : 'b obj -> ('a, 'b, 'd) morph -> 'a obj

  (** Give the identity morphism on an object *)
  val id : ('a, 'a, 'd) morph

  (** Compose two morphisms *)
  val compose :
    'c obj -> ('b, 'c, 'd) morph -> ('a, 'b, 'd) morph -> ('a, 'c, 'd) morph

  (* The following returns weaker than what we want, which is "\exists r.
     allowed * r". But ocaml doesn't like existentials, and this weaker version
     is good enough for us *)

  (** Give left adjoint of a morphism  *)
  val left_adjoint :
    'b obj -> ('a, 'b, 'l * allowed) morph -> ('b, 'a, left_only) morph

  (** Give the right adjoint of a morphism *)
  val right_adjoint :
    'b obj -> ('a, 'b, allowed * 'r) morph -> ('b, 'a, right_only) morph

  include Allow_disallow with type ('a, 'b, 'd) t := ('a, 'b, 'd) morph

  (** Apply morphism on constant *)
  val apply : 'b obj -> ('a, 'b, 'd) morph -> 'a -> 'b

  val print_obj : Format.formatter -> 'a obj -> unit

  (** Print morphism *)
  val print_morph : 'b obj -> Format.formatter -> ('a, 'b, 'd) morph -> unit
end

type positive = private Positive

type negative = private Negative

type 'a pos = 'b * 'c constraint 'a = 'b * 'c

type 'a neg = 'c * 'b constraint 'a = 'b * 'c

module type Polarity = sig
  type polarity

  type 'd polarized constraint 'd = 'l * 'r
end

module type Polarity_ops = sig
  type 'a obj

  type ('a, 'd) mode

  type 'a error

  type polarity

  (** Returns the mode representing the given constant. *)
  val of_const : ('a * polarity) obj -> 'a -> ('a * polarity, 'l * 'r) mode

  (** The minimum mode in the lattice *)
  val min : ('a * polarity) obj -> ('a * polarity, 'l * 'r) mode

  (** The maximum mode in the lattice *)
  val max : ('a * polarity) obj -> ('a * polarity, 'l * 'r) mode

  (** Pushes the mode variable to the lowest constant possible. *)
  val zap_to_floor :
    ('a * polarity) obj -> ('a * polarity, allowed * 'r) mode -> 'a

  (** Pushes the mode variable to the highest constant possible. *)
  val zap_to_ceil :
    ('a * polarity) obj -> ('a * polarity, 'l * allowed) mode -> 'a

  (** Create a new mode variable of the full range. *)
  val newvar : ('a * polarity) obj -> ('a * polarity, 'l * 'r) mode

  (** Try to constrain the first mode below the second mode. *)
  val submode :
    ('a * polarity) obj ->
    ('a * polarity, allowed * 'r) mode ->
    ('a * polarity, 'l * allowed) mode ->
    (unit, 'a error) result

  (** Creates a new mode variable above the given mode and returns [true]. In
        the speical case where the given mode is top, returns the constant top
        and [false]. *)
  val newvar_above :
    ('a * polarity) obj ->
    ('a * polarity, allowed * 'r_) mode ->
    ('a * polarity, 'l * 'r) mode * bool

  (** Creates a new mode variable below the given mode and returns [true]. In
        the speical case where the given mode is bottom, returns the constant
        bottom and [false]. *)
  val newvar_below :
    ('a * polarity) obj ->
    ('a * polarity, 'l_ * allowed) mode ->
    ('a * polarity, 'l * 'r) mode * bool

  (** Returns the join of the list of modes. *)
  val join :
    ('a * polarity) obj ->
    ('a * polarity, allowed * 'r) mode list ->
    ('a * polarity, left_only) mode

  (** Return the meet of the list of modes. *)
  val meet :
    ('a * polarity) obj ->
    ('a * polarity, 'l * allowed) mode list ->
    ('a * polarity, right_only) mode

  (** Checks if a mode has been constrained sufficiently to a constant.
        Expensive. *)
  val check_const :
    ('a * polarity) obj -> ('a * polarity, 'l * 'r) mode -> 'a option

  (** Print a mode. Calls [check_const] for cleaner printing and thus
    expensive.  *)
  val print :
    ?verbose:bool ->
    ('a * polarity) obj ->
    Format.formatter ->
    ('a * polarity, 'l * 'r) mode ->
    unit

  (** Print a mode without calling [check_const]. *)
  val print_raw :
    ?verbose:bool ->
    ('a * polarity) obj ->
    Format.formatter ->
    ('a * polarity, 'l * 'r) mode ->
    unit
end

module type S = sig
  type 'a error =
    { left : 'a;
      right : 'a
    }

  module Magic_allow_disallow (X : Allow_disallow) :
    Allow_disallow with type ('a, 'b, 'd) t := ('a, 'b, 'd) X.t

  (** Solver that supports polarized lattices; needed because some morphisms
      are antitone  *)
  module Solver_polarized (C : Lattices_mono) : sig
    (* Backtracking facilities used by [types.ml] *)

    type changes

    val undo_changes : changes -> unit

    val append_changes : (changes ref -> unit) ref

    (* Construct a new category based on the original category [C]. Objects are
       two copies of the objects in [C] of opposite polarity. The positive copy
       is identical to the original lattice. The negative copy has its lattice
       structure reversed. Morphism are four copies of the morphisms in [C], from
       two copies of objects to two copies of objects. *)

    (** [('a * 'p) obj] identifies an object in the new category, where ['a] is
        the carrier type and ['p] indicates polarity. *)
    type 'a obj =
      | Positive : 'a C.obj -> ('a * positive) obj
          (** The original lattice of obj *)
      | Negative : 'a C.obj -> ('a * negative) obj
          (** the dual lattice of obj *)

    (* A mode with carrier type ['a] and left/right status ['d] derived from the
       morphism it contains. See comments for [morph] for the format of ['d] *)
    type ('a, 'd) mode

    include Allow_disallow with type ('a, _, 'd) t := ('a, 'd) mode

    module Pos :
      Polarity with type polarity = positive and type 'd polarized = 'd pos

    module Neg :
      Polarity with type polarity = negative and type 'd polarized = 'd neg

    (* This is just needed to generate the types for the [apply_*] functions;
       do not use. *)
    module Apply (From : Polarity) (To : Polarity) : sig
      type ('a, 'b, 'd) apply =
        ('b * To.polarity) obj ->
        ('a, 'b, 'd) C.morph ->
        ('a * From.polarity, 'd From.polarized) mode ->
        ('b * To.polarity, 'd To.polarized) mode
    end

    (** The monotone morphism from a positive lattice to a positive lattice *)
    val apply_pos_pos : ('a, 'b, 'd) Apply(Pos)(Pos).apply

    (** The antitone morphism from a positive lattice to a negative lattice *)
    val apply_pos_neg : ('a, 'b, 'd) Apply(Pos)(Neg).apply

    (** The antitone morphism from a negative lattice to a positive lattice *)
    val apply_neg_pos : ('a, 'b, 'd) Apply(Neg)(Pos).apply

    (** The monotone morphism from a negative lattice to a negative lattice *)
    val apply_neg_neg : ('a, 'b, 'd) Apply(Neg)(Neg).apply

    module type Polarity_ops =
      Polarity_ops
        with type 'a obj := 'a obj
         and type ('a, 'd) mode := ('a, 'd) mode
         and type 'a error := 'a error

    module Positive_ops : Polarity_ops with type polarity = positive

    module Negative_ops : Polarity_ops with type polarity = negative
  end
end
