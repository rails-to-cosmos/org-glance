@core
Feature: System Init
  In order to manage things
  As a user
  I want to define things, classes and domains

  Scenario: Let's start from scratch
    When I enter the forest
    Then I am in the buffer "*org-glance*"
    And I should see
      """
      #    -*- mode: org; mode: org-glance-overview -*-

      #+CATEGORY: Forest
      #+STARTUP: overview

      YOU ARE STANDING AT THE END OF A ROAD BEFORE A SMALL BRICK BUILDING.
      AROUND YOU IS A FOREST. A SMALL STREAM FLOWS OUT OF THE BUILDING AND
      DOWN A GULLY.

      WHAT DO YOU SEE? PRESS + TO BREAK A NEW GROUND.
      """
    And I should have 0 grounds owned
    And I should have 0 trees planted
