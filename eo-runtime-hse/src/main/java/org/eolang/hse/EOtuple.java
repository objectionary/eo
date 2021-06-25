package org.eolang.hse;

import org.eolang.hse.core.EOObject;

import java.util.Objects;

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

    /**
     * !!!For testing purposes only!!!
     *
     * Determines if this object is equal to the {@code o} object.
     * To do it, this method checks that the {@code o} object is
     * of the {@code EOtuple} type and the contents of {@code this}
     * and {@code o} are the same.
     *
     * This method can be called only in the testing environment
     * since all methods within the EO environment have the 'EO' prefix.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || !(o instanceof EOtuple)) return false;
        EOtuple tuple = (EOtuple) o;
        return tuple.EOfst.equals(EOfst) && tuple.EOsnd.equals(EOsnd);
    }

    /**
     * !!!For testing purposes only!!!
     *
     * Produces a string that represents this object.
     * The resulting string has the following form:
     * tuple(fst:..., snd:...).
     *
     * Example:
     * tuple(fst:int(100), snd:int(200)).
     *
     * This method can be called only in the testing environment
     * since all methods within the EO environment have the 'EO' prefix.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("tuple(fst:");
        sb.append(EOfst);
        sb.append(", snd:");
        sb.append(EOsnd);
        sb.append(')');
        return sb.toString();
    }

    @Override
    public int hashCode() {
        return Objects.hash(EOfst)+Objects.hash(EOsnd);
    }


}
