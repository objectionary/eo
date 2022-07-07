package EOorg.EOeolang.EOmath;

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sin.
 * @since 1.0
 */
@XmirObject(oname = "angle.sin")
public class EOangle$EOsin extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOangle$EOsin(final Phi sigma) {
        super(sigma);
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> new Data.ToPhi(
                    Math.sin(new Param(rho.attr("ρ").get(), "f").strong(Double.class))
                )
            )
        );
    }
}
