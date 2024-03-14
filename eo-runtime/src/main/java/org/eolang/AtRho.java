package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

public class AtRho implements Attr {
    /**
     * Rho.
     */
    private final AtomicReference<Phi> rho;

    /**
     * Ctor.
     */
    public AtRho() {
        this(null);
    }

    /**
     * Ctor.
     * @param rho Rho.
     */
    private AtRho(final Phi rho) {
        this.rho = new AtomicReference<>(rho);
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtRho(this.rho.get());
    }

    @Override
    public Phi get() {
        if (this.rho.get() == null) {
            throw new ExUnset("");
        }
        return this.rho.get();
    }

    @Override
    public void put(final Phi phi) {
        if (this.rho.get() == null) {
            this.rho.set(phi);
        }
    }

    @Override
    public String φTerm() {
        return null;
    }
}
