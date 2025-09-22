USE heritage;

SELECT c.FirstName, c.LastName
FROM customers c
WHERE c.City = "London" OR c.City = "Berlin";

SELECT p.ProductName
FROM products p
WHERE p.Category = "Electronics" AND p.Price > 500;

SELECT e.FirstName, e.LastName, e.Salary
FROM employees e
ORDER BY e.Salary DESC;

SELECT CustomerID, COUNT(*) AS NumberOfOrders
FROM Orders
GROUP BY CustomerID;

SELECT CustomerID, SUM(Amount) AS TotalAmount
FROM Orders
GROUP BY CustomerID;

SELECT Category, AVG(Price) AS MeanPrice
FROM products
GROUP BY Category;

SELECT Department, COUNT(*) AS NumberOfEmployees
FROM employees
GROUP BY Department;

SELECT CustomerID, SUM(Amount) AS TotalAmount
FROM orders
GROUP BY CustomerID
ORDER BY TotalAmount DESC
LIMIT 1;

SELECT c.FirstName, c.LastName, o.OrderDate
FROM customers c
LEFT JOIN orders o ON c.CustomerID = o.CustomerID;

SELECT p.ProductName
FROM products p
LEFT JOIN orderdetails od ON p.ProductID = od.ProductID
LEFT JOIN orders o ON o.OrderID = od.OrderID
WHERE o.CustomerID = 1;

SELECT c.FirstName, c.LastName, COUNT(o.CustomerID) AS TotalOrders
FROM customers c
LEFT JOIN orders o ON c.CustomerID = o.CustomerID
GROUP BY c.CustomerID, c.FirstName, c.LastName
ORDER BY TotalOrders DESC;

SELECT p.ProductName, od.Quantity, o.OrderID
FROM orders o
LEFT JOIN orderdetails od ON o.OrderID = od.OrderID
LEFT JOIN products p ON od.ProductID = p.ProductID;

SELECT c.CustomerID
FROM customers c
LEFT JOIN orders o ON c.CustomerID = o.CustomerID
WHERE o.OrderID IS NULL;




SELECT c.FirstName, c.LastName, SUM(o.Amount) AS TotalPrice
FROM customers c
LEFT JOIN orders o ON c.CustomerID = o.CustomerID
GROUP BY c.CustomerID, c.FirstName, c.LastName
HAVING SUM(o.Amount) > (SELECT AVG(Amount) FROM orders);



SELECT p.ProductName
FROM products p
LEFT JOIN orderdetails od ON p.ProductID = od.ProductID
LEFT JOIN orders o ON od.OrderID = o.OrderID
WHERE o.OrderID IS NULL;

SELECT c.FirstName, c.LastName, COUNT(DISTINCT od.ProductID) AS ProductNumber
FROM customers c
LEFT JOIN orders o ON c.CustomerID = o.CustomerID
LEFT JOIN orderdetails od ON o.OrderID = od.OrderID
GROUP BY c.CustomerID, c.FirstName, c.LastName
HAVING ProductNumber >= 2;

SELECT e.FirstName, e.LastName
FROM employees e
WHERE e.Salary > (SELECT AVG(e2.Salary)
				   FROM employees e2
				   WHERE e2.Department = e.Department);

SELECT o.OrderID, SUM(od.Quantity) AS TotalQuantity
FROM orders o
LEFT JOIN orderdetails od ON o.OrderID = od.OrderID
GROUP BY o.OrderID
HAVING TotalQuantity > 3;

SELECT c.CustomerID, c.FirstName, c.LastName, SUM(o.Amount) AS TotalSpent,
	CASE WHEN SUM(o.Amount) > 500 THEN "High"
		 WHEN SUM(o.Amount) BETWEEN 200 AND 500 THEN "Medium"
		 ELSE "Low"
	END AS SpendingCategory
FROM customers c
LEFT JOIN orders o ON c.CustomerID = o.CustomerID
GROUP BY c.CustomerID, c.FirstName, c.LastName;


