#ifndef FIELD_H
#define FIELD_H

#include <QIntegerForSize>
#include <QLinkedList>
#include <QPoint>

#define W 9
#define H 9

enum GameType {Lines, Squares, Blocks};

class Field
{
public:
    Field(int width, int height);
    Field(Field *field);
    ~Field();

    int width();
    int height();
    int cell(int x, int y);
    void setCell(int x, int y, int value);
    int colors();
	bool throwBalls(Field *pThrowMap, int nBalls, bool reset = true);
    void clear();
	int count();

    bool hasPath(int x0, int y0, int x, int y, Field *searchResult);
    void shortestPath(int x, int y, Field *searchResult);
    QLinkedList<QPoint> waypoints(int x, int y);
    bool deletionMap(Field *searchResult, GameType gameType);

	void operator += (Field inclusion);


private:
    int *pField;
    int w;
    int h;
    int c;

	int lineLength(int x0, int y0, int dir);
	void markLine(int x0, int y0, int dir, int length);
    int blockSize(int x0, int y0);
};

#endif // FIELD_H
