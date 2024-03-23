package org.eolang;

public class AtData implements Attr {
    /**
     * Original attribute.
     */
    private final Attr origin;

    /**
     * Possible original \rho
     */
    private final Phi parent;

    /**
     * Possible mocked \rho.
     */
    private final Phi rho;

    /**
     * Ctor.
     * @param attr Original attribute
     * @param phi Possible original \rho
     * @param rho Possible mocked \rho
     */
    AtData(final Attr attr, final Phi phi, final Phi rho) {
        this.origin = attr;
        this.parent = phi;
        this.rho = rho;
    }

    @Override
    public Attr copy(final Phi self) {
        throw new IllegalStateException(
            "Should never happen"
        );
    }

    @Override
    public Phi get() {
        return new PhFakeRho(this.origin.get(), this.parent, this.rho);
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
