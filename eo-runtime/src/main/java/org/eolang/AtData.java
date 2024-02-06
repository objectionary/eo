package org.eolang;

public class AtData implements Attr {
    private final Phi phi;

    private final Attr attr;

    public AtData(final Phi phi, final Attr attr) {
        this.attr = attr;
        this.phi = phi;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtData(self, attr);
    }

    @Override
    public Phi get() {
        final Phi got = this.attr.get();
        got.attr(Attr.RHO).put(this.phi);
        return got;
    }

    @Override
    public void put(final Phi phi) {
        this.attr.put(phi);
    }

    @Override
    public String φTerm() {
        return this.attr.φTerm();
    }
}
