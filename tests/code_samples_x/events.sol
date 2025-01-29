// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Events {
    // Define an event for a person
    event PersonCreated(string name, uint256 age, address wallet);

    // Define an event for a product
    event ProductCreated(string name, uint256 price, uint256 stock);

    // State variable to store a person
    struct Person {
        string name;
        uint256 age;
        address wallet;
    }
    Person public person;

    // State variable to store a product
    struct Product {
        string name;
        uint256 price;
        uint256 stock;
    }
    Product public product;

    // Function to create a person and emit an event
    function createPerson(string memory _name, uint256 _age, address _wallet) public {
        person = Person({
            name: _name,
            age: _age,
            wallet: _wallet
        });
        emit PersonCreated(_name, _age, _wallet);
    }

    // Function to create a product and emit an event
    function createProduct(string memory _name, uint256 _price, uint256 _stock) public {
        product = Product({
            name: _name,
            price: _price,
            stock: _stock
        });
        emit ProductCreated(_name, _price, _stock);
    }

    // Function to get the person
    function getPerson() public view returns (string memory, uint256, address) {
        return (person.name, person.age, person.wallet);
    }

    // Function to get the product
    function getProduct() public view returns (string memory, uint256, uint256) {
        return (product.name, product.price, product.stock);
    }
}