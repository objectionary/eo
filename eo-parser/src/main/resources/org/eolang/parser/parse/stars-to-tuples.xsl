<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2025 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="stars-to-tuples" version="2.0">
  <!--
    Converts such XMIR with @star attributes:

    <o star="" base="tuple">
      <o base="1".../>
      <o base="2".../>
      <o base="3".../>
    </o>

    Into the next one without @star:

    <o base="tuple">
      <o base="tuple">
        <o base="tuple">
          <o base="tuple">
          <o base=".empty" method=""/>
          <o base="1"/>
        </o>
        <o base="2"/>
      </o>
      <o base="3"/>
    </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@star]">
    <xsl:choose>
      <xsl:when test="count(o)=0">
        <xsl:choose>
          <xsl:when test="count(following-sibling::o)=0 or following-sibling::o[1][not(@base) or @base!='.empty']">
            <o>
              <xsl:for-each select="@*[name()!='star' and name()!='name']">
                <xsl:attribute name="{name()}">
                  <xsl:value-of select="."/>
                </xsl:attribute>
              </xsl:for-each>
            </o>
            <o base=".empty" method="">
              <xsl:if test="./@name">
                <xsl:attribute name="name">
                  <xsl:value-of select="./@name"/>
                </xsl:attribute>
              </xsl:if>
              <xsl:attribute name="line">
                <xsl:value-of select="./@line"/>
              </xsl:attribute>
              <xsl:attribute name="pos">
                <xsl:value-of select="./@pos+1"/>
              </xsl:attribute>
            </o>
          </xsl:when>
          <xsl:otherwise>
            <o>
              <xsl:for-each select="@*[name()!='star']">
                <xsl:attribute name="{name()}">
                  <xsl:value-of select="."/>
                </xsl:attribute>
              </xsl:for-each>
            </o>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="count(o[not(@method)])=1">
        <o>
          <xsl:for-each select="@*[name()!='star']">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <o base="tuple"/>
          <o base=".empty" method=""/>
          <xsl:copy-of select="o"/>
        </o>
      </xsl:when>
      <xsl:when test="count(o[not(@method)])&gt;1">
        <o>
          <xsl:for-each select="@*[name()!='star']">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <o base="tuple" star="">
            <xsl:for-each select="o[not(@method)][last()]/preceding-sibling::o">
              <xsl:copy-of select="."/>
            </xsl:for-each>
          </o>
          <xsl:copy-of select="o[not(@method)][last()] | o[not(@method)][last()]/following-sibling::o"/>
        </o>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
