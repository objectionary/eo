package EOorg.EOeolang.EOgray;

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;
import org.eolang.gray.Ram;

@XmirObject(oname = "ram.read")
public class EOram$EOread extends PhDefault {
    public EOram$EOread(Phi sigma) {
        super(sigma);
        this.add("p", new AtFree());
        this.add("l", new AtFree());
        this.add("φ", new AtComposite(this, rho -> {
            final int pos = new Param(rho, "p").strong(Long.class).intValue();
            final int len = new Param(rho, "l").strong(Long.class).intValue();
            return new Data.ToPhi(
                Ram.INSTANCE.read(
                    rho.attr("ρ").get(), pos, len)
            );
        }));
    }


}
