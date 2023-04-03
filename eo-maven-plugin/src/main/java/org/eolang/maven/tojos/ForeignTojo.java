package org.eolang.maven.tojos;

import com.yegor256.tojos.Tojo;
import java.nio.file.Path;
import java.nio.file.Paths;

public class ForeignTojo {

    private final Tojo delegate;

    public ForeignTojo(final Tojo delegate) {
        this.delegate = delegate;
    }

    public Path xmir2(){
        return Paths.get(this.delegate.get(ForeignTojos.Attribute.XMIR_2.key()));
    }
    public Path eolangObject() {
        return Paths.get(this.delegate.get(ForeignTojos.Attribute.EO.key()));
    }
}
