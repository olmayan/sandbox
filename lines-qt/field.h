#ifndef FIELD_H
#define FIELD_H

#include <QIntegerForSize>
#include <QLinkedList>
#include <QPoint>

#define W 9
#define H 9

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
    bool throwBalls(Field *throwMap, int nBalls);
    void clear();

    bool hasPath(int x0, int y0, int x, int y, Field *searchResult);
    void shortestPath(int x, int y, Field *searchResult);
    QLinkedList<QPoint> waypoints(int x, int y);

private:
    int *pField;
    int w;
    int h;
    int c;
};

#endif // FIELD_H
