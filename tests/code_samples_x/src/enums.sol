// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Enums {
    // Define an enum for different states
    enum State { Pending, Active, Inactive, Completed }

    // Define an enum for different user roles
    enum Role { Admin, User, Guest }

    // State variable to store the current state
    State public currentState;

    // State variable to store the current role
    Role public currentRole;

    // Function to set the state
    function setState(State _state) public {
        currentState = _state;
    }

    // Function to set the role
    function setRole(Role _role) public {
        currentRole = _role;
    }

    // Function to get the current state
    function getState() public view returns (State) {
        return currentState;
    }

    // Function to get the current role
    function getRole() public view returns (Role) {
        return currentRole;
    }
}