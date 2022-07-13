/*
 * @checkstyle PackageNameCheck (4 lines)
 */
package EOorg.EOeolang;

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Read from memory.
 * @since 0.1
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "ram.read")
public class EOram$EOread extends PhDefault {
    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOram$EOread(Phi sigma) {
        super(sigma);
        this.add("p", new AtFree());
        this.add("l", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final int pos = new Param(rho, "p").strong(Long.class).intValue();
                    final int len = new Param(rho, "l").strong(Long.class).intValue();
                    return new Data.ToPhi(
                        Ram.INSTANCE.read(
                            rho.attr("ρ").get(), pos, len)
                    );
                }
            )
        );
    }


}
