<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2024 Objectionary.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="add_rust" version="2.0">
  <!--
  Creates <rusts> section with <rust> inserts.
  -->
  <xsl:template match="program">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <rusts>
        <xsl:comment>"Rust inserts"</xsl:comment>
        <xsl:for-each select="//o">
          <xsl:if test="../attribute(base) = '.rust' and attribute(base) = 'org.eolang.string'">
            <rust>
              <xsl:attribute name="code">
                <xsl:value-of select="./o/text()"/>
              </xsl:attribute>
              <xsl:attribute name="code_loc">
                <xsl:value-of select="attribute(loc)"/>
              </xsl:attribute>
              <dependencies>
                <xsl:for-each select="following-sibling::o/o">
                  <xsl:if test="parent::o[@base = 'org.eolang.tuple'] and @base = 'org.eolang.string'">
                    <dependency>
                      <xsl:attribute name="name">
                        <xsl:value-of select="./o/text()"/>
                      </xsl:attribute>
                    </dependency>
                  </xsl:if>
                </xsl:for-each>
              </dependencies>
            </rust>
          </xsl:if>
        </xsl:for-each>
      </rusts>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
