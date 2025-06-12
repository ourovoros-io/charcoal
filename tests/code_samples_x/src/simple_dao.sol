// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract SimpleDAO {
    struct Proposal {
        string description;
        uint256 voteCount;
        bool executed;
    }

    address public chairperson;
    mapping(address => bool) public members;
    Proposal[] public proposals;

    event ProposalCreated(uint256 proposalId, string description);
    event Voted(address indexed voter, uint256 proposalId);
    event ProposalExecuted(uint256 proposalId);

    modifier onlyChairperson() {
        require(msg.sender == chairperson, "Only chairperson can call this function");
        _;
    }

    modifier onlyMember() {
        require(members[msg.sender], "Only members can call this function");
        _;
    }

    constructor() {
        chairperson = msg.sender;
        members[msg.sender] = true;
    }

    function addMember(address member) public onlyChairperson {
        members[member] = true;
    }

    function createProposal(string memory description) public onlyMember {
        proposals.push(Proposal({
            description: description,
            voteCount: 0,
            executed: false
        }));
        emit ProposalCreated(proposals.length - 1, description);
    }

    function vote(uint256 proposalId) public onlyMember {
        require(proposalId < proposals.length, "Invalid proposal ID");
        require(!proposals[proposalId].executed, "Proposal already executed");

        proposals[proposalId].voteCount += 1;
        emit Voted(msg.sender, proposalId);
    }

    function executeProposal(uint256 proposalId) public onlyChairperson {
        require(proposalId < proposals.length, "Invalid proposal ID");
        require(!proposals[proposalId].executed, "Proposal already executed");
        require(proposals[proposalId].voteCount > 0, "No votes for this proposal");

        proposals[proposalId].executed = true;
        emit ProposalExecuted(proposalId);
    }

    function getProposal(uint256 proposalId) public view returns (string memory description, uint256 voteCount, bool executed) {
        require(proposalId < proposals.length, "Invalid proposal ID");
        Proposal storage proposal = proposals[proposalId];
        return (proposal.description, proposal.voteCount, proposal.executed);
    }
}