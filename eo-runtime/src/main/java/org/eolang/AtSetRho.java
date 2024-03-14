package org.eolang;

public class AtSetRho implements Attr {
    /**
     * Original attribute.
     */
    private final Attr origin;

    /**
     * Rho to put.
     */
    private final Phi rho;

    private final String name;

    /**
     * Ctor.
     * @param attr Origin attribute
     * @param rho \Rho
     * @param name Name of the attribute
     */
    public AtSetRho(final Attr attr, final Phi rho, final String name) {
        this.origin = attr;
        this.rho = rho;
        this.name = name;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtSetRho(this.origin.copy(self), self, this.name);
    }

    @Override
    public Phi get() {
        final Phi ret = this.origin.get();
        if (!this.name.equals(Attr.RHO) && !this.name.equals(Attr.SIGMA)) {
            ret.attr(Attr.RHO).put(rho);
        }
        return ret;
    }

    @Override
    public void put(final Phi phi) {
        this.origin.put(phi);
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
