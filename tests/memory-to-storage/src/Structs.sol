// SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.13;

contract Structs {
    AX public ax = AX({x: "x"});

    struct AX {
        string x;
    }

    event OX(string);

    function z(AX memory _ax) public {
        emit OX(_ax.x);
        ax = _ax;
    }

    function f() public {
        emit OX(ax.x);
    }
}
