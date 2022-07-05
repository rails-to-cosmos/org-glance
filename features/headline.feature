Feature: Headline
  Scenario: Create from headline at point: point at title
    Given headline "sre"
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """
    Then the title of headline "sre" should be "Google SRE Book"
    And the contents of headline "sre" should be:
      """
      * Google SRE Book :book:article:
      Managing Incidents
      """
    And headline "sre" should be a book
    And headline "sre" should be an article

  Scenario: Create from headline at point: point inside subtree
    Given file "tasks.org"
      """
      * TODO Vocal lesson :Task:
      - Mozart
      - Bach
      """
    And I find file "tasks.org"
    And I goto the end of the buffer
    And I create headline "vocals" from element at point
    Then the title of headline "vocals" should be "Vocal lesson"
    And headline "vocals" should be a task
    And headline "vocals" contents should be:
      """
      * TODO Vocal lesson :Task:
      - Mozart
      - Bach
      """

  Scenario: Serializable contents
    Given headline "lesson"
      """
      *** Driving lesson :Education:
      - Key: Value
      """
    And I save headline "lesson" to file "driving.org"
    And I load headline "driving" from file "driving.org"
    Then the title of headline "driving" should be "Driving lesson"
    And headline "driving" should be equal to headline "lesson"

  Scenario: Normalized indentation
    Given headline "indented"
      """
      *** Tags :article:
      Features and scenarios can be tagged using syntax @tag
      **** More indentations
      """
    Given headline "unindented"
      """
      * Tags :article:
      Features and scenarios can be tagged using syntax @tag
      ** More indentations
      """
    Then headline "indented" should be equal to headline "unindented"

  Scenario: No links in title
    Given headline "bookmark"
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """
    Then the title of headline "bookmark" should be "Transmission bookmark: Transmission Web UI"
    And headline "bookmark" should be a bookmark
    And headline "bookmark" should contain link
    And headline "bookmark" contents should be:
      """
      * Transmission bookmark: [[http://127.0.0.1:9091/transmission/web/#upload][Transmission Web UI]] :bookmark:
      - Uses fewer resources than other clients
      """

  Scenario: Headline contains links
    Given headline "bookmark"
      """
      * Release notes :Bookmark:
      - [[https://www.apple.com/workspace.htm][Release Notes]]
      """
    Then headline "bookmark" should contain links

  Scenario: Headline is encrypted
    Given headline "bookmark"
      """
      * Release notes :Bookmark:
      aes-encrypted V 1.3-OCB-B-4-4-M
      ttT0N8RLii13eMJ+R25Z/o42dnw9M10+qZDOzH3PjMGzuVQYSxGuDZ2n8UUHwRvsQhgsizOGi4dE
      u65keAuJ/R1oskxb4dkIjFIW2ZLxoghkkb8j6xbLL9I=
      """
    Then headline "bookmark" should be encrypted

  Scenario: Headline contains properties specified by user
    Given headline "credentials"
      """
      * Credentials
      Login: admin
      Password: admin
      """
    Then headline "credentials" should be propertized

  Scenario: Archived headline
    Given headline "archive"
      """
      * Old stuff :ARCHIVE:
      """
    Then headline "archive" should be archived

  Scenario: Commented headline
    Given headline "comment"
      """
      * COMMENT Implicit stuff
      """
    Then headline "comment" should be commented

  Scenario: Closed headline
    Given headline "done"
      """
      * DONE Hard work
      CLOSED: [2022-02-05 Sat 19:09]
      :LOGBOOK:
      - State "DONE"       from "TODO"       [2022-02-05 Sat 19:09]
      :END:
      """
    Then headline "done" should be closed

  Scenario: Materialize single headline once
    Given file "origin.org"
      """
      * iPhone 3 :phone:
      """

    Given file "material.org"
      """
      """

    When I find file "origin.org"
    And I create headline "iphone" from element at point
    And I kill buffer
    And I materialize headline "iphone" to file "material.org"
    And I find file "material.org"
    And I go to the first headline
    Then I set title of the headline at point to "iPhone 4"
    And I commit changes
    And I save buffer
    And I find file "origin.org"
    Then buffer string should be
      """
      * iPhone 4 :phone:
      """

  Scenario: Materialize single headline twice
    Given file "origin.org"
      """
      * iPhone 3 :phone:
      """

    Given file "material.org"
      """
      """

    When I find file "origin.org"
    And I create headline "iphone" from element at point
    And I kill buffer
    And I materialize headline "iphone" to file "material.org"
    And I find file "material.org"
    And I go to the first headline
    Then I set title of the headline at point to "iPhone 4"
    And I save buffer
    Then I set title of the headline at point to "iPhone 5"
    And I save buffer
    And I find file "origin.org"
    Then buffer string should be
      """
      * iPhone 5 :phone:
      """

  Scenario: Materialize multiple headlines
    Given file "origin.org"
      """
      * iPhone 3 :phone:
      * Samsung Galaxy Note 8 :phone:
      """

    Given file "material.org"
      """
      """

    When I find file "origin.org"
    And I create headline "iphone" from element at point
    And I goto the end of the buffer
    And I create headline "samsung" from element at point
    And I kill buffer

    And I materialize headlines "iphone, samsung" to file "material.org"
    And I find file "material.org"
    And I go to the first headline
    Then I set title of the headline at point to "iPhone 4"
    And I save buffer

    And I find file "origin.org"
    Then buffer string should be
      """
      * iPhone 4 :phone:
      * Samsung Galaxy Note 8 :phone:
      """

  Scenario: Materialize headline with contents before the first headline
    Given file "origin.org"
      """
      Some contents before the first headline.

      * iPhone 3 :phone:
      """

    Given file "material.org"
      """
      """

    When I find file "origin.org"
    And I go to the first headline
    And I create headline "iphone" from element at point
    And I kill buffer
    And I materialize headline "iphone" to file "material.org"
    And I find file "material.org"
    And I go to the first headline
    Then I set title of the headline at point to "iPhone 4"
    And I commit changes
    And I save buffer
    And I find file "origin.org"
    Then buffer string should be
      """
      Some contents before the first headline.

      * iPhone 4 :phone:
      """

#   Scenario: Materialize encrypted headline
#   Scenario: Materialize multiple headlines and add a new one
#   Scenario: Materialize non-file headline
#   Scenario: Materialize multiple headlines, some encrypted, some not, some non-file
