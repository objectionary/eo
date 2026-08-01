Consider this code:

```
[] > app
  inc t > @
  [] > t
    [] > next
  [x] > inc
    x.next.foo > @
```

Type inference should find a problem in this code:
The `t` object does have `next` attribute,
  but the object attached to it doesn't have `foo`.
That's why the last line in the snippet should raise a type mistake.

This is how we can implement this.

First, we get XMIR out of this code:

```
<o name="app">
  <o base="ξ.inc" name="φ">
     <o as="α0" base="ξ.t"/>
  </o>
  <o name="inc">
     <o base="∅" name="x"/>
     <o base="ξ.x.next.foo" name="φ"/>
  </o>
  <o name="t">
     <o name="next"/>
  </o>
</o>
```

Then, we unroll all composite `@base` attributes (that have more than one dot):

```
<o name="app">
  <o base="ξ.inc" name="φ">
    <o as="α0" base="ξ.t"/>
  </o>
  <o name="inc">
    <o base="∅" name="x"/>
    <o base=".foo" name="φ">
      <o base=".next">
        <o base="ξ.x"/>
      </o>
    </o>
  </o>
  <o name="t">
    <o name="next"/>
  </o>
</o>
```

Then, we attach synthetic types to every `<o>` object:

```
<o name="app" type="t0">
  <o base="ξ.inc" name="φ" type="t1">
    <o as="α0" base="ξ.t" type="t2"/>
  </o>
  <o name="inc" type="t3">
    <o base="∅" name="x" type="t4"/>
    <o base=".foo" name="φ" type="t5">
      <o base=".next" type="t6">
        <o base="ξ.x" type="t7"/>
      </o>
    </o>
  </o>
  <o name="t" type="t8">
    <o name="next" type="t9"/>
  </o>
</o>
```

Every `<o>` keeps its own type forever; we never merge or rename them.
Instead, all knowledge about types goes into three collections:

* the **Provides** table: what an object certainly has;
* the **Requires** table: what an object must have,
  judging by how it is used;
* the **Links** list: which types are copies of which.

We fill them in one pass through the XMIR, using four simple rules.

First, a **formation** tells us what its object provides, for example:

```
<o name="t" type="t8">
  <o name="next" type="t9"/>
</o>
```

This XMIR fragment adds two rows to the Provides table:

```
t8: has next (of type t9); complete
t9: has nothing; complete
```

The word "complete" means we have seen the whole formation,
  so there is nothing in it besides the listed attributes.
The other two formations add their own rows:

```
t0: has φ (t1), inc (t3), t (t8); complete
t3: has x (t4, void), φ (t5); complete
```

Second, a **reference** (a `@base` pointing to `ξ.something`) adds a link;
  for example, `base="ξ.t"` links t2 with t8,
  the type of the `t` formation it points to.
After the pass, the Links list is:

```
t1 is a copy of t3
t2 is a copy of t8
t7 is a copy of t4
```

For now, "a copy of" simply means "the same as":
  whatever one of them has or requires, the other one does too.
We keep links as separate records,
  instead of renaming t8 into t2 right in the XMIR, on purpose:
  later, this is the one place where the checker gets smarter
  (a copy may receive its own fresh types),
  and no other part of the pipeline will have to change.

Third, a **dispatch** tells us what an object must have, for example:

```
<o base=".next" type="t6">
  <o base="ξ.x" type="t7"/>
</o>
```

Here somebody takes `next` from the object of type t7,
  therefore t7 must have such an attribute.
This adds a row to the Requires table;
  the `.foo` dispatch wrapped around this fragment adds one more:

```
t7: needs next (of type t6)
t6: needs foo (of type t5)
```

Fourth, an **application** creates a pending check, for example:

```
<o base="ξ.inc" name="φ" type="t1">
  <o as="α0" base="ξ.t" type="t2"/>
</o>
```

The check says: "t2 must fit into the first void attribute of t1."
We don't try to resolve it immediately — maybe we don't know enough yet.

Now, the checking itself.
All pending checks go into a to-do list, and we loop over it:

* take a check from the list;
* if we know enough to decide, decide:
  either everything is fine (possibly producing new, smaller checks),
  or it is a type mistake;
* if we don't know enough yet, put the check back and take the next one;
* stop when the list is empty or no check can make progress anymore.

We also remember every check we have already started
  and never start the same one twice;
  this keeps the loop finite even when objects refer to themselves.

For our example, the loop makes three steps:

1. "t2 must fit into the first void of t1":
  t1 is a copy of t3, and t3 has one void attribute, x, of type t4;
  the check turns into "t2 must have everything that t4 requires."
2. t4 is a copy of t7, and t7 needs next (of type t6);
  t2 is a copy of t8, and t8 does have next (of type t9);
  fine so far, and the check turns into
  "t9 must have everything that t6 requires."
3. t6 needs foo, while t9 has nothing and is complete.
  This is the type mistake:
  `x.next.foo` asks for `foo`, which doesn't exist.

One last rule keeps the checker honest:
  we report a mistake only when we are sure.
"Sure" means: the type on the "has" side is complete,
  and the needed attribute is still not there.
If the type is not complete (an atom, an object from another file),
  or the check never collects enough information to be decided,
  we stay silent.
It is better to miss a mistake than to complain about correct code.

This design can grow without being rebuilt:

* delegation to φ becomes one more place to look
  when we ask "does this type have the attribute?";
* "a copy of" stops meaning "the same as"
  and starts meaning "a fresh duplicate with its own types" —
  only the link-resolution rule changes;
* atoms and objects from other files arrive
  as ready-made rows in the Provides table.

At its core, this design is the textbook recipe:
  give every expression its own type variable,
  walk the program collecting facts about these variables,
  then solve the facts —
  exactly how Hindley–Milner inference works in ML or Haskell.
Our Provides table with its "complete" flag is what textbooks call
  record types with closed and open rows.
The Requires table and the pending checks are subtyping constraints.
The to-do loop is a standard worklist constraint solver.
Remembering started checks is the usual trick
  that makes recursive types terminate.
The closest published relative is type inference
  for the Abadi–Cardelli object calculus,
  a close cousin of our φ-calculus.

It differs from the textbooks in three places,
  each time for an EO-specific reason.
Textbooks mostly solve equations ("these two types are equal"),
  while we accumulate one-directional facts
  ("this type must have that attribute"),
  because dispatch in EO only cares that an attribute is present,
  not that two types match exactly.
Textbooks type every expression or reject the whole program,
  while we let a check stay undecided and keep silent,
  because parts of any real EO program are invisible to the checker:
  atoms, objects from other files, delegation through φ.
Textbook object typing leans on classes and inheritance,
  while EO has neither,
  so our types are nothing but sets of attributes —
  what the literature calls structural typing.

End.
