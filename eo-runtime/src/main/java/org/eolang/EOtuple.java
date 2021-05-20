package org.eolang;

import org.eolang.core.EOObject;

/** Package-scope object 'tuple'. */
public class EOtuple extends EOObject {

    /** Field for storing the 'fst' free attribute. */
    private final EOObject EOfst;
    /** Field for storing the 'snd' free attribute. */
    private final EOObject EOsnd;

    /**
     * Constructs (via one-time-full application) the package-scope object 'tuple'.
     *
     * @param EOfst the object to bind to the 'fst' free attribute.
     * @param EOsnd the object to bind to the 'snd' free attribute.
     */
    public EOtuple(EOObject EOfst, EOObject EOsnd) {
        this.EOfst = EOfst;
        this.EOsnd = EOsnd;
    }

    /** Returns the object bound to the 'fst' input attribute. */
    public EOObject EOfst() {
        return this.EOfst;
    }

    /** Returns the object bound to the 'snd' input attribute. */
    public EOObject EOsnd() {
        return this.EOsnd;
    }
}
