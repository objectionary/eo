package org.eolang.maven.optimization;

import com.jcabi.xml.XML;
import java.nio.file.Path;

public interface Optimization {

    XML optimize(Path xml) throws OptimizationException;

}
