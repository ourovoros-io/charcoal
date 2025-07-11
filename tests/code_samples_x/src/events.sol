// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Events {
    // Define an event for a person
    event PersonCreated(uint256 age, address wallet);

    // Define an event for a product
    event ProductCreated(uint256 price, uint256 stock);

    // State variable to store a person
    struct Person {
        uint256 age;
        address wallet;
    }
    Person public person;

    // State variable to store a product
    struct Product {
        uint256 price;
        uint256 stock;
    }
    Product public product;

    // Function to create a person and emit an event
    function createPerson(uint256 _age, address _wallet) public {
        person = Person({age: _age, wallet: _wallet});
        emit PersonCreated(_age, _wallet);
    }

    // Function to create a product and emit an event
    function createProduct(uint256 _price, uint256 _stock) public {
        product = Product({price: _price, stock: _stock});
        emit ProductCreated(_price, _stock);
    }

    // Function to get the person
    function getPerson() public view returns (uint256, address) {
        return (person.age, person.wallet);
    }

    // Function to get the product
    function getProduct() public view returns (uint256, uint256) {
        return (product.price, product.stock);
    }
}
