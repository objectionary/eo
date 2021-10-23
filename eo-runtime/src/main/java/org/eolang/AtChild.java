package org.eolang;

/**
 * When a child object is taken from the \phi object, this class
 * replaces the \rho attribute of it on the fly.
 *
 * @since 0.1
 */
final class AtChild implements Attr {
    /**
     * The \phi attribute.
     */
    private final Attr base;
    /**
     * The name.
     */
    private final String name;
    /**
     * The parent to put into \rho attribute of the original object.
     */
    private final Phi parent;
    /**
     * Ctor.
     * @param attr The origin
     * @param prnt The value of \rho to use
     */
    AtChild(final Attr phi, final String attr, final Phi prnt) {
        this.base = phi;
        this.name = attr;
        this.parent = prnt;
    }
    @Override
    public Attr copy(final Phi self) {
        return new AtChild(this.base.copy(self), this.name, this.parent);
    }
    @Override
    public Phi get() {
        final Phi phi = this.base.get().attr(this.name).get();
        if (!(phi instanceof Data)) {
            phi.attr("ρ").put(this.parent);
        }
        return phi;
    }
    @Override
    public void put(final Phi phi) {
        this.base.get().attr(this.name).put(phi);
    }
}
