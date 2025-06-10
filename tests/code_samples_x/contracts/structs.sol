// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Structs {
    // Define a struct for a Person
    struct Person {
        string name;
        uint256 age;
        address wallet;
    }

    // Define a struct for a Product
    struct Product {
        string name;
        uint256 price;
        uint256 stock;
    }

    // State variable to store a person
    Person public person;

    // State variable to store a product
    Product public product;

    // Function to set the person
    function setPerson(string memory _name, uint256 _age, address _wallet) public {
        person = Person({
            name: _name,
            age: _age,
            wallet: _wallet
        });
    }

    // Function to set the product
    function setProduct(string memory _name, uint256 _price, uint256 _stock) public {
        product = Product({
            name: _name,
            price: _price,
            stock: _stock
        });
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