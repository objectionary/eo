package org.eolang;

public abstract class PhDecorator implements Phi{

    /**
     * The original.
     */
    protected final Phi origin;

    PhDecorator(final Phi phi) {
        this.origin = phi;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }
}
