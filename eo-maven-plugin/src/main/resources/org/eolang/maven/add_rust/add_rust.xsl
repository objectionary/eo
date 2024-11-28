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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="add_rust" version="2.0">
  <!--
  Creates <rusts> section with <rust> inserts.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:template match="program">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <rusts>
        <xsl:comment>"Rust inserts"</xsl:comment>
        <xsl:for-each select="//o">
          <xsl:if test="(../attribute(base) = 'org.eolang.rust' or (../attribute(base) = '.rust' and ../o[1]/attribute(base) = '.eolang' and ../o[1]/o[1]/attribute(base) = '.org' and ../o[1]/o[1]/o[1]/attribute(base) = 'Q')) and (attribute(base) = 'org.eolang.string' or (attribute(base) = '.string' and o[1]/attribute(base) = '.eolang' and o[1]/o[1]/attribute(base) = '.org' and o[1]/o[1]/o[1]/attribute(base) = 'Q'))">
            <rust>
              <xsl:attribute name="code">
                <xsl:value-of select="./o[eo:has-data(.)]/text()"/>
              </xsl:attribute>
              <xsl:attribute name="code_loc">
                <xsl:value-of select="attribute(loc)"/>
              </xsl:attribute>
              <dependencies>
                <xsl:for-each select="following-sibling::o/o">
                  <xsl:if test="(parent::o/attribute(base) = 'org.eolang.tuple' or (parent::o/attribute(base) = '.tuple' and parent::o/o[1]/attribute(base) = '.eolang' and parent::o/o[1]/o[1]/attribute(base) = '.org' and parent::o/o[1]/o[1]/o[1]/attribute(base) = 'Q')) and (attribute(base) = 'org.eolang.string' or (attribute(base) = '.string' and o[1]/attribute(base) = '.eolang' and o[1]/o[1]/attribute(base) = '.org' and o[1]/o[1]/o[1]/attribute(base) = 'Q'))">
                    <dependency>
                      <xsl:attribute name="name">
                        <xsl:value-of select="./o[eo:has-data(.)]/text()"/>
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
