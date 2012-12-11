<!-- Hey, emacs, please consider this to be -*- xml -*-
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:macro="http://lichteblau.com/macro"
                version="1.0">
  <!--
      Symbol index
    -->

  <xsl:template match="*" mode="symbol-index"/>

  <xsl:template match="external-symbols" mode="symbol-index">
    <xsl:param name="packagep"/>
    <simple-table>
      <xsl:apply-templates mode="symbol-index">
      <xsl:sort select="@name" data-type="text" order="ascending"/>
      <xsl:with-param name="packagep" select="$packagep"/>
      </xsl:apply-templates>
    </simple-table>
  </xsl:template>

  <xsl:template name="index-entry">
    <xsl:param name="packagep"/>
    <xsl:param name="kind"/>
    <row>
      <xsl:if test="$packagep">
        <cell align="right" nowrap="nowrap">
          <span class="nonlink">
            <tt>
              <span style="color: #777777">
                <xsl:value-of select="../../@name"/>
                <xsl:text>:</xsl:text>
              </span>
            </tt>
          </span>
        </cell>
      </xsl:if>
      <cell>
        <a>
          <xsl:choose>
            <xsl:when test="/documentation/@single-page-p">
              <xsl:attribute name="href">
                <xsl:text>#</xsl:text>
                <xsl:value-of select="@id"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="href">
                <xsl:value-of select="$packagep"/>
                <xsl:value-of select="@id"/>
                <xsl:text>.html</xsl:text>
              </xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <span class="code">
            <xsl:value-of select="@name"/>
          </span>
        </a>
        <xsl:text>, </xsl:text>
        <xsl:value-of select="$kind"/>
        <xsl:call-template name="undocumented"/>
      </cell>
    </row>
  </xsl:template>

  <xsl:template match="symbol-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'symbol'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="variable-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'variable'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="function-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'function'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="generic-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'generic function'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="operator-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'special operator'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="macro-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'macro'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="type-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'type'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="class-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'class'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="systemclass-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'systemclass'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="struct-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'struct'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="condition-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'condition'"/>
    </xsl:call-template>
  </xsl:template>

  <!-- General Fonts -->

  <xsl:template match="em">
    <i>
      <xsl:apply-templates/>
    </i>
  </xsl:template>

  <xsl:template match="b">
    <b>
      <xsl:apply-templates/>
    </b>
  </xsl:template>

  <!-- Fonts Used in the Common Lisp Hyperspec -->

  <xsl:template match="term">
    <span class="term">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template match="defterm">
    <span class="defterm">
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  
  <xsl:template match="sym">
    <span class="sym">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template match="code">
    <span class="code">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template match="arg">
    <span class="arg">
        <xsl:apply-templates/>
    </span>
  </xsl:template>

<!-- Formating of text -->

  <xsl:template match="file">
    <tt><em>
      <xsl:apply-templates/>
    </em></tt>
  </xsl:template>

  <xsl:template match="break">
    <br/><br/>
  </xsl:template>

  <xsl:template match="br">
      <br/>
  </xsl:template>

  <xsl:template match="pre">
    <pre>
      <xsl:apply-templates/>
    </pre>
  </xsl:template>

  <xsl:template match="indent">
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="itemize">
    <ul>
      <xsl:apply-templates/>
    </ul>
  </xsl:template>

  <xsl:template match="enumerate">
    <ol>
      <xsl:apply-templates/>
    </ol>
  </xsl:template>

  <xsl:template match="item">
    <li>
      <xsl:apply-templates/>
    </li>
  </xsl:template>

  <xsl:template match="table">
    <dl>
      <xsl:apply-templates/>
    </dl>
  </xsl:template>

  <xsl:template match="entry">
    <dt>
      <tt>
        <xsl:value-of select="@entry"/>
      </tt>
    </dt>
    <dd>
      <xsl:apply-templates/>
    </dd>
  </xsl:template>

<!-- Cross references to Common lisp symbols -->

  <xsl:template match="symbol">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="var">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="fun">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="gen">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="operator">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="macro">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="type">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="class">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="systemclass">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="struct">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="condition">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

</xsl:stylesheet>

